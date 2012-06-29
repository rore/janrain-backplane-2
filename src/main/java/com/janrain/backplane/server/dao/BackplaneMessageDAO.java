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

import com.janrain.backplane.server.BackplaneMessage;
import com.janrain.backplane.server.BackplaneServerException;
import com.janrain.backplane.server.config.Backplane1Config;
import com.janrain.backplane.server.redis.Redis;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import com.janrain.commons.util.SerializationUtils;
import com.yammer.metrics.Metrics;
import com.yammer.metrics.core.Histogram;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import redis.clients.jedis.Jedis;
import redis.clients.jedis.Pipeline;
import redis.clients.jedis.Response;

import java.util.*;

/**
 * @author Tom Raney
 */
public class BackplaneMessageDAO extends DAO<BackplaneMessage> {

    final public static String V1_MESSAGE_QUEUE = "v1_message_queue";
    final public static String V1_MESSAGES = "v1_messages";

    public static byte[] getBusKey(String bus) {
        return ("v1_" + bus).getBytes();
    }

    public static byte[] getChannelKey(String bus, String channel) {
        return ("v1_" + bus + "_" + channel).getBytes();
    }

    public static byte[] getMessageIdKey(String bus, String channel, String id) {
        return (new String(getChannelKey(bus, channel)) + "_" + id).getBytes();
    }


    @Override
    public void persist(BackplaneMessage message) throws SimpleDBException {

        //superSimpleDB.store(bpConfig.getMessagesTableName(), BackplaneMessage.class, message, true);

        //TODO: pipeline the rest.  No reason to serialize these ops

        String id = message.getIdValue();
        String bus = message.get(BackplaneMessage.Field.BUS);
        String channel = message.get(BackplaneMessage.Field.CHANNEL_NAME);

        Redis.getInstance().rpush(getBusKey(bus), getMessageIdKey(bus, channel, id));
        Redis.getInstance().set(getMessageIdKey(bus, channel, id), SerializationUtils.toBytes(message));

        //TODO: ttl?
        //append message to list of messages in a channel
        Redis.getInstance().rpush(getChannelKey(bus, channel), SerializationUtils.toBytes(message));

        //TODO: add ttl here?
        Redis.getInstance().rpush(V1_MESSAGES.getBytes(), id.getBytes());
    }

    /**
     * Add message to work queue - any node may add since it is an atomic operation
     * However, the message ID will be determined later by the message processor
     * @param message
     */
    public void addToQueue(BackplaneMessage message) {
        Redis.getInstance().rpush(V1_MESSAGE_QUEUE.getBytes(), SerializationUtils.toBytes(message));
    }

    @Override
    public void delete(String id) throws SimpleDBException {

    }

    public BackplaneMessage get(String key) {
        return SerializationUtils.fromBytes(Redis.getInstance().get(key.getBytes()));
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
                BackplaneMessage message = SerializationUtils.fromBytes(b);
                if (message != null) {
                    messages.add(message);
                }
            }
        }

        filterAndSort(messages, since, sticky);
        return messages;
    }

    public List<String> getMessageIds(List<BackplaneMessage> messages) {
        List<String> ids = new ArrayList<String>();
        for (BackplaneMessage message : messages) {
            ids.add(message.getIdValue());
        }
        return ids;
    }

    public List<BackplaneMessage> getMessagesByBus(String bus, String since, String sticky) throws SimpleDBException, BackplaneServerException {

        Jedis jedis = null;

        try {
            jedis = Redis.getInstance().getJedis();

            Date sinceDate = BackplaneMessage.parseIdDate(since);

            // every message has a unique timestamp - which serves as a key for indexing
            Set<byte[]> messageIdBytes = Redis.getInstance().zrangebyscore(BackplaneMessageDAO.getBusKey(bus), sinceDate == null ? 0 : sinceDate.getTime(), Double.POSITIVE_INFINITY);

            List<BackplaneMessage> messages = new ArrayList<BackplaneMessage>();

            Pipeline pipeline = jedis.pipelined();
            List<Response<byte[]>> responses = new ArrayList<Response<byte[]>>();

            if (messageIdBytes != null) {
                for (byte[] b: messageIdBytes) {
                    responses.add(pipeline.get(b));
                }
                pipeline.sync();
                for (Response<byte[]> response: responses) {
                    BackplaneMessage message = SerializationUtils.fromBytes(response.get());
                    if (message != null) {
                        messages.add(message);
                    }
                }
            }

            filterAndSort(messages, since, sticky);
            return messages;

        } finally {
            Redis.getInstance().releaseToPool(jedis);
        }
    }

    // - PACKAGE

    BackplaneMessageDAO(SuperSimpleDB superSimpleDB, Backplane1Config bpConfig) {
        super(superSimpleDB, bpConfig);
    }

    // - PRIVATE

    private static final Logger logger = Logger.getLogger(BackplaneMessageDAO.class);

    private final Histogram messagesPerChannel = Metrics.newHistogram(BackplaneMessageDAO.class, "v1_messages_per_channel");

    private void filterAndSort(List<BackplaneMessage> messages, String since, String sticky) {

        // filter per sticky flag
        if (StringUtils.isNotBlank(sticky)) {
            Iterator<BackplaneMessage> iterator = messages.iterator();
            while(iterator.hasNext()) {
                BackplaneMessage message = iterator.next();
                if (!message.get(BackplaneMessage.Field.STICKY.getFieldName()).equals(sticky)) {
                    iterator.remove();
                }
            }
        }

        // filter per since flag
        if (StringUtils.isNotBlank(since)) {
            Iterator<BackplaneMessage> iterator = messages.iterator();
            while(iterator.hasNext()) {
                BackplaneMessage message = iterator.next();
                if (message.getIdValue().compareTo(since) <= 0) {
                    iterator.remove();
                }
            }
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
    }

    private String genBusKey(String key) {
        return "v1bus" + key;
    }

    private String genChannelKey(String key) {
        return "v1channel" + key;
    }
}
