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
import redis.clients.jedis.*;

import java.util.*;

/**
 * @author Tom Raney
 */
public class MessageProcessor extends JedisPubSub {

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

    public void subscribe() {

        Jedis jedis = null;
        try {
            jedis = Redis.getInstance().getJedis();
            //this call is blocking
            jedis.subscribe(this, "alerts");
        } finally {
            Redis.getInstance().releaseToPool(jedis);
        }
    }

    /**
     * Processor to pull messages off queue and make them available
     *
     */
    public void doWork() {

        // retrieve as many messages as possible from the queue
        String uuid = UUID.randomUUID().toString();

        try {
            logger.info("message processor waiting for exclusive write lock");

            // TRY forever to get lock to do work
            String lock = Redis.getInstance().getLock(V1_WRITE_LOCK, uuid, -1, 30);

            // if we lose our lock sometime between this point and the lock refresh, the
            // transaction will fail

            if (lock != null) {
                logger.info("message processor got lock " + lock);
            } else {
                logger.warn("something went terribly wrong");
                return;
            }

            while(true) {

                Jedis jedis = null;

                try {

                    jedis = Redis.getInstance().getJedis();

                    // retrieve the latest 'live' message ID
                    String latestMessageId = "";
                    Set<String> latestMessageIdSet = jedis.zrange(BackplaneMessageDAO.V1_MESSAGES, -1, -1);
                    if (latestMessageIdSet != null && !latestMessageIdSet.isEmpty()) {
                        latestMessageId = latestMessageIdSet.iterator().next();
                    }

                    // retrieve a handful of messages (ten) off the queue for processing
                    List<byte[]> messagesToProcess = jedis.lrange(BackplaneMessageDAO.V1_MESSAGE_QUEUE.getBytes(), 0, 9);

                    // only enter the next block if we have messages to process
                    if (messagesToProcess.size() > 0) {

                        // set watch on the lock key - this will allow the transaction to abort if
                        // the lock is reset by another process
                        jedis.watch(V1_WRITE_LOCK);

                        Transaction transaction = jedis.multi();

                        int inserts = 0;

                        for (byte[] messageBytes : messagesToProcess) {

                            if (messageBytes != null) {
                                BackplaneMessageNew bmn = BackplaneMessageNew.fromBytes(messageBytes);

                                if (bmn != null) {

                                    // the id is set by the node that queued the message - record
                                    // how long the message was in the queue - we assume here that the time
                                    // to post and make the message available is minimal

                                    {
                                        Date insertionTime = BackplaneMessageNew.getDateFromId(bmn.getId());
                                        long now = System.currentTimeMillis();
                                        long diff = now - insertionTime.getTime();
                                        if (diff >= 0) {
                                            timeInQueue.update(now-insertionTime.getTime());
                                        }
                                    }

                                    // TOTAL ORDER GUARANTEE
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
                                    // because of the TOTAL ORDER mechanism above
                                    long messageTime = BackplaneMessageNew.getDateFromId(bmn.getId()).getTime();

                                    // <ATOMIC>
                                    // save the individual message by key
                                    transaction.set(bmn.getId().getBytes(), bmn.toBytes());

                                    // append entire message to list of messages in a channel for retrieval efficiency
                                    transaction.rpush(BackplaneMessageDAO.getChannelKey(bmn.getBus(), bmn.getChannel()), bmn.toBytes());

                                    // add message id to sorted set of all message ids as an index
                                    transaction.zadd(BackplaneMessageDAO.V1_MESSAGES.getBytes(), messageTime, bmn.getId().getBytes());

                                    // add message id to sorted set keyed by bus as an index
                                    transaction.zadd(BackplaneMessageDAO.getBusKey(bmn.getBus()), messageTime, bmn.getId().getBytes());

                                    // make sure all subscribers get the update
                                    transaction.publish("alerts", bmn.getId());

                                    // pop one message off the queue - which will only happen if this transaction is successful
                                    transaction.lpop(BackplaneMessageDAO.V1_MESSAGE_QUEUE);
                                    // </ATOMIC>

                                    logger.info("message " + bmn.getId() + " pushed");
                                    inserts++;

                                    // update the 'latest' message id with the one just inserted
                                    latestMessageId = bmn.getId();

                                }
                            }
                        }

                        logger.info("processing transaction with " + inserts + " message(s)");
                        if (transaction.exec() == null) {
                            // the transaction failed, which likely means the lock was lost
                            logger.warn("transaction failed! - halting work for now");
                            return;
                        }

                    }

                    if (!Redis.getInstance().refreshLock(V1_WRITE_LOCK, uuid, 30)) {
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
            // we may have already lost the lock, but if we exit for any other reason, good to release it
            Redis.getInstance().releaseLock(V1_WRITE_LOCK, uuid);
        }
    }

    private static final String V1_WRITE_LOCK = "v1_write_lock";

    private static final Logger logger = Logger.getLogger(MessageProcessor.class);

    private final Histogram timeInQueue = Metrics.newHistogram(MessageProcessor.class, "time_in_queue");
}
