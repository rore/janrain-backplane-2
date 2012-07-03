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

package com.janrain.backplane2.server;

import com.janrain.backplane2.server.config.BusConfig2;
import com.janrain.backplane2.server.dao.redis.RedisBackplaneMessageDAO;
import com.janrain.redis.Redis;
import com.yammer.metrics.Metrics;
import com.yammer.metrics.core.Histogram;
import org.apache.commons.lang.SerializationUtils;
import org.apache.log4j.Logger;
import redis.clients.jedis.Jedis;
import redis.clients.jedis.JedisPubSub;
import redis.clients.jedis.Transaction;

import java.util.*;

/**
 * @author Tom Raney
 */
public class V2MessageProcessor extends JedisPubSub {

    public V2MessageProcessor() {}

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
     * Processor to remove expired messages
     */
    public void cleanupMessages() {

        Jedis jedis = null;

        try {

            logger.info("preparing to cleanup v2 messages");

            jedis = Redis.getInstance().getJedis();

            Set<byte[]> messageMetaBytes = jedis.zrangeByScore(RedisBackplaneMessageDAO.V2_MESSAGES.getBytes(), 0, Double.MAX_VALUE);
            if (messageMetaBytes != null) {
                Iterator it = messageMetaBytes.iterator();
                while (it.hasNext()) {
                    byte[] b = (byte[]) it.next();

                    String metaData = new String(b);
                    String[] segs = metaData.split(" ");
                    if (jedis.get(segs[2]) == null) {
                        // remove this key from indexes
                        logger.info("removing message " + segs[2]);
                        jedis.zrem(RedisBackplaneMessageDAO.getBusKey(segs[0]), segs[2].getBytes());
                        jedis.lrem(RedisBackplaneMessageDAO.getChannelKey(segs[1]), 0, segs[2].getBytes());
                        jedis.zrem(RedisBackplaneMessageDAO.V2_MESSAGES.getBytes(), segs[2].getBytes());
                        //todo: remove the empty set?
                    }
                }
            }
        } catch (Exception e) {
            logger.error(e);
        } finally {
            logger.info("exiting message cleanup");
            Redis.getInstance().releaseToPool(jedis);
        }


    }

    /**
     * Processor to pull messages off queue and make them available
     *
     */
    public void insertMessages(boolean loop) {

        String uuid = UUID.randomUUID().toString();

        try {
            logger.info("v2 message processor waiting for exclusive write lock");

            // TRY forever to get lock to do work
            String lock = Redis.getInstance().getLock(V2_WRITE_LOCK, uuid, -1, 30);

            // if we lose our lock sometime between this point and the lock refresh, the
            // transaction will fail

            if (lock != null) {
                logger.info("v2 message processor got lock " + lock);
            } else {
                logger.warn("something went terribly wrong");
                return;
            }

            do {

                Jedis jedis = null;

                try {

                    jedis = Redis.getInstance().getJedis();

                    // retrieve the latest 'live' message ID
                    String latestMessageId = "";
                    Set<String> latestMessageMetaSet = jedis.zrange(RedisBackplaneMessageDAO.V2_MESSAGES, -1, -1);
                    if (latestMessageMetaSet != null && !latestMessageMetaSet.isEmpty()) {
                        String[] segs = latestMessageMetaSet.iterator().next().split(" ");
                        latestMessageId = segs[2];
                    }

                    // retrieve a handful of messages (ten) off the queue for processing
                    List<byte[]> messagesToProcess = jedis.lrange(RedisBackplaneMessageDAO.V2_MESSAGE_QUEUE.getBytes(), 0, 9);

                    // only enter the next block if we have messages to process
                    if (messagesToProcess.size() > 0) {

                        // set watch on the lock key - this will allow the transaction to abort if
                        // the lock is reset by another process
                        jedis.watch(V2_WRITE_LOCK);

                        Transaction transaction = jedis.multi();

                        int inserts = 0;

                        for (byte[] messageBytes : messagesToProcess) {

                            if (messageBytes != null) {
                                BackplaneMessage backplaneMessage = (BackplaneMessage) SerializationUtils.deserialize(messageBytes);

                                if (backplaneMessage != null) {

                                    // retrieve the expiration config per the bus
                                    BusConfig2 busConfig2 = null;//DAOFactory.getInstance().getBusDAO().get(backplaneMessage.getBus());
                                    int retentionTimeSeconds = 60;
                                    int retentionTimeStickySeconds = 3600;
                                    if (busConfig2 != null) {
                                        // should be here in normal flow
                                        retentionTimeSeconds = busConfig2.getRetentionTimeSeconds();
                                        retentionTimeStickySeconds = busConfig2.getRetentionTimeStickySeconds();
                                    }

                                    // the id is set by the node that queued the message - record
                                    // how long the message was in the queue - we assume here that the time
                                    // to post and make the message available is minimal

                                    {
                                        long insertTime = BackplaneMessage.getDateFromId(backplaneMessage.getIdValue()).getTime();
                                        long now = System.currentTimeMillis();
                                        long diff = now - insertTime;
                                        if (diff >= 0 && diff < 2880000) {
                                            timeInQueue.update(diff);
                                        } else {
                                            logger.warn("time diff is bizarre at: " + diff);
                                        }
                                    }

                                    // TOTAL ORDER GUARANTEE
                                    // verify that the new message ID is greater than all existing message IDs
                                    // if not, uptick id by 1 ms and insert
                                    // this means that all message ids have unique time stamps, even if they
                                    // arrived at the same time.

                                    if (backplaneMessage.getIdValue().compareTo(latestMessageId) <= 0) {
                                        logger.warn("new message has an id " + backplaneMessage.getIdValue() + " that is not > the latest id of " + latestMessageId);
                                        Date lastMessageDate = BackplaneMessage.getDateFromId(latestMessageId);
                                        if (lastMessageDate != null) {
                                            backplaneMessage.setIdValue(com.janrain.backplane.server.BackplaneMessage.generateMessageId(new Date(lastMessageDate.getTime() + 1)));
                                            logger.warn("fixed");
                                        } else {
                                            logger.warn("could not modify id of new message");
                                        }
                                    }

                                    // messageTime is guaranteed to be a unique identifier of the message
                                    // because of the TOTAL ORDER mechanism above
                                    long messageTime = BackplaneMessage.getDateFromId(backplaneMessage.getIdValue()).getTime();

                                    // <ATOMIC>
                                    // save the individual message by key
                                    transaction.set(RedisBackplaneMessageDAO.getKey(backplaneMessage.getIdValue()), SerializationUtils.serialize(backplaneMessage));
                                    // set the message TTL
                                    if (backplaneMessage.isSticky()) {
                                        transaction.expire(backplaneMessage.getIdValue().getBytes(), retentionTimeStickySeconds);
                                    } else {
                                        transaction.expire(backplaneMessage.getIdValue().getBytes(), retentionTimeSeconds);
                                    }

                                    // append entire message to list of messages in a channel for retrieval efficiency
                                    transaction.rpush(RedisBackplaneMessageDAO.getChannelKey(backplaneMessage.getChannel()),
                                            SerializationUtils.serialize(backplaneMessage));

                                    // add message id to sorted set of all message ids as an index
                                    String metaData = backplaneMessage.getBus() + " " + backplaneMessage.getChannel() + " " + backplaneMessage.getIdValue();
                                    transaction.zadd(RedisBackplaneMessageDAO.V2_MESSAGES.getBytes(), messageTime, metaData.getBytes());

                                    // add message id to sorted set keyed by bus as an index
                                    transaction.zadd(RedisBackplaneMessageDAO.getBusKey(backplaneMessage.getBus()), messageTime, backplaneMessage.getIdValue().getBytes());

                                    // make sure all subscribers get the update
                                    transaction.publish("alerts", backplaneMessage.getIdValue());

                                    // pop one message off the queue - which will only happen if this transaction is successful
                                    transaction.lpop(RedisBackplaneMessageDAO.V2_MESSAGE_QUEUE);
                                    // </ATOMIC>

                                    logger.info("v2 message " + backplaneMessage.getIdValue() + " pushed");
                                    inserts++;

                                    // update the 'latest' message id with the one just inserted
                                    latestMessageId = backplaneMessage.getIdValue();

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

                    if (!Redis.getInstance().refreshLock(V2_WRITE_LOCK, uuid, 30)) {
                        logger.warn("lost lock! - halting work for now");
                        return;
                    }

                } finally {
                    Redis.getInstance().releaseToPool(jedis);
                }

            } while (loop);

        } catch (Exception e) {
            logger.warn("exception thrown in message processor thread", e);
        } finally {
            logger.info("v2 message processor releasing lock");
            // we may have already lost the lock, but if we exit for any other reason, good to release it
            Redis.getInstance().releaseLock(V2_WRITE_LOCK, uuid);
        }

    }

    private static final String V2_WRITE_LOCK = "v2_write_lock";

    private static final Logger logger = Logger.getLogger(V2MessageProcessor.class);

    private final Histogram timeInQueue = Metrics.newHistogram(V2MessageProcessor.class, "time_in_queue");
}
