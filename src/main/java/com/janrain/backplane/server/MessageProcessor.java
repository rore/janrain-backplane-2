/*
 * Copyright 2012 Janrain, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.janrain.backplane.server;

import com.janrain.backplane.server.dao.BackplaneMessageDAO;
import com.janrain.backplane.server.redis.Redis;
import com.yammer.metrics.Metrics;
import com.yammer.metrics.core.Histogram;
import org.apache.log4j.Logger;
import redis.clients.jedis.Jedis;
import redis.clients.jedis.JedisPubSub;
import redis.clients.jedis.Pipeline;
import redis.clients.jedis.Response;

import java.util.*;

/**
 * @author Tom Raney
 */
public class MessageProcessor extends JedisPubSub {

    public static boolean messageProcessorRunning = false;
    public static boolean subscriberRunning = false;

    public MessageProcessor() {}

    @Override
    public void onMessage(String s, String s1) {
        logger.info("message received on channel: " + s + " with message: " + s1);
    }

    @Override
    public void onPMessage(String s, String s1, String s2) {

    }

    @Override
    public void onSubscribe(String s, int i) {
        logger.info("successfully subscribed to " + s);
    }

    @Override
    public void onUnsubscribe(String s, int i) {

    }

    @Override
    public void onPUnsubscribe(String s, int i) {

    }

    @Override
    public void onPSubscribe(String s, int i) {

    }

    public synchronized void subscribe() {

        subscriberRunning = true;

        Jedis jedis = null;

        try {
            jedis = Redis.getInstance().getJedis();
            //this call is blocking
            jedis.subscribe(this, "alerts");
        } finally {
            Redis.getInstance().releaseToPool(jedis);
            subscriberRunning = false;
        }

    }

    /**
     * Processor to pull messages off queue and make them available
     *
     */
    public void doWork() {

        // retrieve as many messages as possible from the queue

        messageProcessorRunning = true;

        String uuid = UUID.randomUUID().toString();

        try {
            logger.info("message processor waiting for exclusive write lock");

            // TRY forever to get lock to do work
            String lock = Redis.getInstance().getLock("write", uuid, -1, 30);
            // todo: if one processing loop takes longer than 30s, what prevents some else from getting the lock?

            if (lock != null) {
                logger.info("message processor got lock " + lock);
            } else {
                logger.warn("something went terribly wrong");
                return;
            }

            while(true) {

                Jedis jedis = null;

                try {

                    // retrieve the latest message ID from Redis
                    jedis = Redis.getInstance().getJedis();
                    Pipeline pipeline = jedis.pipelined();

                    Response<Set<String>> latestMessageIdSet = pipeline.zrange(BackplaneMessageDAO.V1_MESSAGES.getBytes(), -1, -1);
                    List<Response<byte[]>> queue = new ArrayList<Response<byte[]>>();

                    // pop a handful of messages off queue for processing
                    for (int i=0; i< 10; i++ ) {
                        queue.add(pipeline.lpop(BackplaneMessageDAO.V1_MESSAGE_QUEUE.getBytes()));
                    }

                    // go!
                    pipeline.sync();

                    //TODO: if we crash here, these messages would be lost...
                    //pop may not be the best solution here

                    String latestMessageId = "";
                    if (latestMessageId != null && !latestMessageIdSet.get().isEmpty()) {
                        Set<String> set = latestMessageIdSet.get();
                        latestMessageId = set.iterator().next();
                    }

                    // pipeline the writes
                    int inserts = 0;

                    for (Response<byte[]> response : queue) {
                        byte[] responseBytes = response.get();

                        if (responseBytes != null) {
                            BackplaneMessageNew bmn = BackplaneMessageNew.fromBytes(responseBytes);

                            if (bmn != null) {

                                // the id is set by the node that queued the message - record
                                // how long the message was in the queue - we assume here that the time
                                // to post and make the message available is minimal

                                {
                                    Date insertionTime = BackplaneMessageNew.getDateFromId(bmn.getId());
                                    long now = System.currentTimeMillis();
                                    long diff = now - insertionTime.getTime();
                                    if (diff >= 0) {
                                        timeInQueue.update(now-insertionTime.getTime()); // todo: why 'now' and not on pipeline.sync()
                                    }
                                }

                                // verify that the new message ID is greater than all existing message IDs
                                // if not, uptick id by 1 ms and insert
                                // this means that all message ids have unique time stamps, even if they
                                // arrived at the same time.

                                if (bmn.getId().compareTo(latestMessageId) <= 0) {
                                    logger.warn("new message has an id " + bmn.getId() + " that is not > the latest id of " + latestMessageId);
                                    Date lastMessageDate = BackplaneMessageNew.getDateFromId(latestMessageId);
                                    if (lastMessageDate != null) {
                                        bmn.setId(BackplaneMessageNew.generateMessageId(new Date(lastMessageDate.getTime()+1)));
                                        logger.warn("fixed");
                                    } else {
                                        logger.warn("could not modify id of new message");
                                    }
                                }

                                // messageTime is guaranteed to be a unique identifier of the message
                                long messageTime = BackplaneMessageNew.getDateFromId(bmn.getId()).getTime();

                                // stuff the message id into a sorted set keyed by bus
                                pipeline.zadd(BackplaneMessageDAO.getBusKey(bmn.getBus()), messageTime, bmn.getId().getBytes());

                                // save the individual message by key
                                pipeline.set(bmn.getId().getBytes(), bmn.toBytes()); // todo: are messages persisted...

                                //TODO: ttl?
                                // append message to list of messages in a channel
                                pipeline.rpush(BackplaneMessageDAO.getChannelKey(bmn.getBus(), bmn.getChannel()), bmn.toBytes()); // todo: ... twice, by design?
                                pipeline.set(bmn.getId().getBytes(), bmn.toBytes());

                                //TODO: ttl?
                                // append message to list of messages in a channel
                                pipeline.rpush(BackplaneMessageDAO.getChannelKey(bmn.getBus(), bmn.getChannel()), bmn.toBytes());

                                // add message id to sorted set of all message ids
                                pipeline.zadd(BackplaneMessageDAO.V1_MESSAGES.getBytes(), messageTime, bmn.getId().getBytes());

                                // make sure all subscribers get the update
                                pipeline.publish("alerts", bmn.getId());

                                logger.info("message " + bmn.getId() + " pushed");
                                inserts++;

                                latestMessageId = bmn.getId();

                            }
                        }
                    }

                    if (inserts > 0) {
                        logger.info("flushing pipeline with " + inserts + " messages");
                        pipeline.sync();
                    }

                    if (!Redis.getInstance().refreshLock("write", uuid, 30)) {
                        logger.warn("lost lock! - halting work for now");
                        return;
                    }

                } finally {
                    Redis.getInstance().releaseToPool(jedis);
                }

            }

        } catch (Exception e) {
            logger.warn("exception thrown in message processor thread", e);
        } finally {
            logger.info("message processor releasing lock");
            Redis.getInstance().releaseLock("write", uuid);
            messageProcessorRunning = false;
        }

    }

    private static final Logger logger = Logger.getLogger(MessageProcessor.class);

    private final Histogram timeInQueue = Metrics.newHistogram(MessageProcessor.class, "time_in_queue");
}
