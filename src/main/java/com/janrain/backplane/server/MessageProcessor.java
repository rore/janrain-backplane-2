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

import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.UUID;

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
     */
    public synchronized void doWork() {

        // retrieve as many messages as possible from the queue

        messageProcessorRunning = true;

        String uuid = UUID.randomUUID().toString();

        // CRITICAL SECTION
        synchronized ("write".intern()) {

            try {
                logger.info("message processor waiting for exclusive write lock");

                // TRY forever to get lock to do work
                String lock = Redis.getInstance().getLock("write", uuid, -1, 30);

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

                        Response<List<String>> latestMessageIdList = pipeline.lrange(BackplaneMessageDAO.V1_MESSAGES.getBytes(), -1, -1);
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
                        if (latestMessageId != null && !latestMessageIdList.get().isEmpty()) {
                            latestMessageId = new String(latestMessageIdList.get().get(0));
                        }

                        // pipeline the writes
                        int inserts = 0;

                        for (Response<byte[]> response : queue) {
                            byte[] responseBytes = response.get();

                            if (responseBytes != null) {
                                BackplaneMessageNew bmn = BackplaneMessageNew.fromBytes(responseBytes);

                                if (bmn != null) {
                                    // set messageID
                                    bmn.setId(Backplane1Controller.generateMessageId());

                                    //2012-06-22T21:38:12.065Z-a0c92c9624 that is not > the latest id of 2012-06-22T21:38:12.065Z-b8c4014d1a

                                    {
                                        Date insertionTime = BackplaneMessageNew.getDateFromId(bmn.getId());
                                        long now = System.currentTimeMillis();
                                        long diff = now - insertionTime.getTime();
                                        if (diff >= 0) {
                                            timeInQueue.update(now-insertionTime.getTime());
                                        }
                                    }

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

                                    pipeline.rpush(BackplaneMessageDAO.getBusKey(bmn.getBus()),
                                            BackplaneMessageDAO.getMessageIdKey(bmn.getBus(), bmn.getChannel(), bmn.getId()));
                                    pipeline.set(BackplaneMessageDAO.getMessageIdKey(bmn.getBus(),
                                            bmn.getChannel(), bmn.getId()), bmn.toBytes());

                                    //TODO: ttl?
                                    //append message to list of messages in a channel
                                    pipeline.rpush(BackplaneMessageDAO.getChannelKey(bmn.getBus(), bmn.getChannel()), bmn.toBytes());

                                    //TODO: add ttl here?
                                    pipeline.rpush(BackplaneMessageDAO.V1_MESSAGES.getBytes(), bmn.getId().getBytes());

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

                        //Thread.sleep(100);
                        //logger.info("looping");
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
    }

    private static final Logger logger = Logger.getLogger(MessageProcessor.class);


    private final Histogram timeInQueue = Metrics.newHistogram(MessageProcessor.class, "time_in_queue");
}
