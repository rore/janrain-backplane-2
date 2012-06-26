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

package com.janrain.backplane.server.dao;

import com.janrain.backplane.server.migrate.legacy.BackplaneMessage;
import com.janrain.backplane.server.BackplaneMessageNew;
import com.janrain.backplane.server.BackplaneServerException;
import com.janrain.backplane.server.config.Backplane1Config;
import com.janrain.backplane.server.redis.Redis;
import com.janrain.cache.CachedL1;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import com.janrain.locking.DistributedLockingManager;
import com.yammer.metrics.Metrics;
import com.yammer.metrics.core.Histogram;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import redis.clients.jedis.Jedis;
import redis.clients.jedis.Pipeline;
import redis.clients.jedis.Response;

import java.io.*;
import java.util.*;

/**
 * @author Tom Raney
 */
public class BackplaneMessageDAO extends DAO<BackplaneMessage> {

    final public static String V1_MESSAGE_QUEUE = "v1_message_queue";
    final public static String V1_MESSAGES = "v1_messages";

    public static byte[] getBusKey(String bus) {
        return new String("v1_" + bus).getBytes();
    }

    public static byte[] getChannelKey(String bus, String channel) {
        return new String("v1_" + bus + "_" + channel).getBytes();
    }

    public static byte[] getMessageIdKey(String bus, String channel, String id) {
        return new String( new String(getChannelKey(bus, channel)) + "_" + id).getBytes();
    }


    @Override
    public void persist(BackplaneMessage message) throws SimpleDBException {

        //superSimpleDB.store(bpConfig.getMessagesTableName(), BackplaneMessage.class, message, true);

        BackplaneMessageNew bmn = new BackplaneMessageNew(message);

        //TODO: pipeline the rest.  No reason to serialize these ops

        Redis.getInstance().rpush(getBusKey(bmn.getBus()), getMessageIdKey(bmn.getBus(), bmn.getChannel(), bmn.getId()));
        Redis.getInstance().set(getMessageIdKey(bmn.getBus(), bmn.getChannel(), bmn.getId()), bmn.toBytes());

        //TODO: ttl?
        //append message to list of messages in a channel
        Redis.getInstance().rpush(getChannelKey(bmn.getBus(), bmn.getChannel()), bmn.toBytes());

        //TODO: add ttl here?
        Redis.getInstance().rpush(V1_MESSAGES.getBytes(), bmn.getId().getBytes());

    }



    /**
     * Add message to work queue - any node may add since it is an atomic operation
     * However, the message ID will be determined later by the message processor
     * @param message
     */

    public void addToQueue(BackplaneMessage message) {
        BackplaneMessageNew backplaneMessageNew = new BackplaneMessageNew(message);
        Redis.getInstance().rpush(V1_MESSAGE_QUEUE.getBytes(), backplaneMessageNew.toBytes());
    }

    @Override
    public void delete(String id) throws SimpleDBException {

    }

    public BackplaneMessage get(String key) {
        byte[] messageBytes = Redis.getInstance().get(key.getBytes());
        if (messageBytes != null) {
            BackplaneMessageNew backplaneMessageNew = BackplaneMessageNew.fromBytes(messageBytes);
            if (backplaneMessageNew != null) {
                try {
                    return backplaneMessageNew.convertToOld();
                } catch (SimpleDBException e) {

                } catch (BackplaneServerException e) {

                }
            }
        }
        return null;
    }

    public boolean canTake(String bus, String channel, int msgPostCount) throws SimpleDBException {

        long count = Redis.getInstance().llen(getChannelKey(bus, channel));

        logger.debug("channel: '" + channel + "' message count: " + count + ", limit: " + bpConfig.getDefaultMaxMessageLimit());

        return count + msgPostCount < bpConfig.getDefaultMaxMessageLimit();

    }

    /**
     * Fetch a list (possibly empty) of backplane messages that exist on the channel
     * and return them in order by message id
     * @param channel
     * @return
     */

    public List<BackplaneMessage> getMessagesByChannel(String bus, String channel, String since, String sticky) throws SimpleDBException, BackplaneServerException {

        List<byte[]> messageBytes = Redis.getInstance().lrange(getChannelKey(bus, channel), 0, -1);

        List<BackplaneMessage> messages = new ArrayList<BackplaneMessage>();
        if (messageBytes != null) {
            for (byte[] b: messageBytes) {
                BackplaneMessageNew bmn = BackplaneMessageNew.fromBytes(b);
                if (bmn != null) {
                    messages.add(bmn.convertToOld());
                }
            }
        }

        return filterAndSort(messages, since, sticky);

    }

    public List<String> getMessageIds(List<BackplaneMessage> messages) {
        List<String> ids = new ArrayList<String>();
        for (BackplaneMessage message : messages) {
            ids.add(message.getIdValue());
        }
        return ids;
    }

    public List<BackplaneMessage> getMessagesByBus(String bus, String since, String sticky) throws SimpleDBException, BackplaneServerException {

        List<byte[]> messageIdBytes = Redis.getInstance().lrange(getBusKey(bus), 0, -1);

        List<BackplaneMessage> messages = new ArrayList<BackplaneMessage>();

        Jedis jedis = Redis.getInstance().getJedis();
        Pipeline pipeline = jedis.pipelined();
        List<Response> responses = new ArrayList<Response>();

        try {
            if (messageIdBytes != null) {
                for (byte[] b: messageIdBytes) {
                    responses.add(pipeline.get(b));
                }
                pipeline.sync();
                for (Response<byte[]> response: responses) {
                    BackplaneMessageNew bmn = BackplaneMessageNew.fromBytes(response.get());
                    if (bmn != null) {
                        messages.add(bmn.convertToOld());
                    }
                }
            }
        } finally {
            Redis.getInstance().releaseToPool(jedis);
        }

        return filterAndSort(messages, since, sticky);

    }

    // - PACKAGE

    BackplaneMessageDAO(SuperSimpleDB superSimpleDB, Backplane1Config bpConfig, com.janrain.backplane.server.dao.DaoFactory daoFactory) {
        super(superSimpleDB, bpConfig);
        this.daoFactory = daoFactory;
    }

    // - PRIVATE

    private static final Logger logger = Logger.getLogger(BackplaneMessageDAO.class);

    private final Histogram messagesPerChannel = Metrics.newHistogram(BackplaneMessageDAO.class, "v1_messages_per_channel");

    private final DaoFactory daoFactory;

    private List<BackplaneMessage> filterAndSort(List<BackplaneMessage> messages, String since, String sticky) {

        // filter per sticky flag
        if (StringUtils.isNotBlank(sticky)) {
            for (BackplaneMessage message: messages) {
                if (!message.get(BackplaneMessage.Field.STICKY.getFieldName()).equals(sticky)) {
                    messages.remove(message);
                }
            }
        }

        // filter per since flag
        if (StringUtils.isNotBlank(since)) {
            List<BackplaneMessage> newList = new ArrayList<BackplaneMessage>();
            for (BackplaneMessage msg: messages) {
                if (msg.getIdValue().compareTo(since) > 0) {
                    newList.add(msg);
                }
            }
            messages = newList;
        }

        Collections.sort(messages, new Comparator<BackplaneMessage>() {
            @Override
            public int compare(BackplaneMessage backplaneMessage, BackplaneMessage backplaneMessage1) {
                if (backplaneMessage == backplaneMessage1) {
                    return 0;
                }
                return backplaneMessage.getIdValue().compareTo(backplaneMessage1.getIdValue());
            }
        });

        return messages;

    }

    private String genBusKey(String key) {
        return "v1bus" + key;
    }

    private String genChannelKey(String key) {
        return "v1channel" + key;
    }


}
