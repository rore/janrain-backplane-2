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

package com.janrain.backplane2.server.dao.redis;

import com.janrain.backplane2.server.*;
import com.janrain.backplane2.server.dao.BackplaneMessageDAO;
import com.janrain.oauth2.TokenException;
import com.janrain.redis.Redis;
import org.apache.commons.lang.NotImplementedException;
import org.apache.commons.lang.SerializationUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import redis.clients.jedis.Jedis;
import redis.clients.jedis.Pipeline;
import redis.clients.jedis.Response;
import redis.clients.jedis.Transaction;

import javax.servlet.http.HttpServletResponse;
import java.util.*;

/**
 * @author Tom Raney, Johnny Bufu
 */
public class RedisBackplaneMessageDAO implements BackplaneMessageDAO {

    final public static String V2_MESSAGE_QUEUE = "v2_message_queue";
    final public static String V2_MESSAGES = "v2_messages";

    public static byte[] getBusKey(String bus) {
        return ("v2_bus_idx_" + bus).getBytes();
    }

    public static byte[] getChannelKey(String channel) {
        return ("v2_channel_idx_" + channel).getBytes();
    }

    public static byte[] getKey(String key) {
        return ("v2_message_" + key).getBytes();
    }


    @Override
    public BackplaneMessage getLatestMessage() throws BackplaneServerException {
        List<byte[]> bytesList = Redis.getInstance().lrange(V2_MESSAGES.getBytes(), -1, -1);
        if (! bytesList.isEmpty()) {
            return (BackplaneMessage) SerializationUtils.deserialize(bytesList.get(0));
        } else {
            return null;
        }
    }

    @NotNull
    @Override
    public BackplaneMessage retrieveBackplaneMessage(@NotNull String messageId, @NotNull Token token)
            throws BackplaneServerException, TokenException {

        BackplaneMessage message = get(messageId);

        if ( message == null || ! token.getScope().isMessageInScope(message)) {
            // don't disclose that the messageId exists if not in scope
            throw new TokenException("Message id '" + messageId + "' not found", HttpServletResponse.SC_NOT_FOUND);
        } else {
            return message;
        }
    }

    @Override
    public long getMessageCount(String channel) {
        return (int) Redis.getInstance().llen(getChannelKey(channel));
    }

    @Override
    public long countMessages() throws BackplaneServerException {
        return Redis.getInstance().llen(V2_MESSAGES.getBytes());
    }

    @Override
    public void retrieveMessagesPerScope(@NotNull MessagesResponse bpResponse, @NotNull Token token) throws BackplaneServerException {
        final Scope scope = token.getScope();

        List<BackplaneMessage> messages = retrieveMessagesNoScope(bpResponse.getLastMessageId());
        filterMessagesPerScope(messages, scope, bpResponse);
    }

    @Override
    public List<BackplaneMessage> retrieveMessagesNoScope(@Nullable String sinceIso8601timestamp) throws BackplaneServerException {
        Jedis jedis = null;

        try {

            jedis = Redis.getInstance().getJedis();

            double sinceInMs = 0;
            if (StringUtils.isNotBlank(sinceIso8601timestamp)) {
                sinceInMs = BackplaneMessage.getDateFromId(sinceIso8601timestamp).getTime();
            }

            // every message has a unique timestamp - which serves as a key for indexing
            Set<byte[]> messageIdBytes = jedis.zrangeByScore(V2_MESSAGES.getBytes(), sinceInMs+1, Double.POSITIVE_INFINITY);

            List<BackplaneMessage> messages = new ArrayList<BackplaneMessage>();

            Pipeline pipeline = jedis.pipelined();
            List<Response<byte[]>> responses = new ArrayList<Response<byte[]>>();

            if (messageIdBytes != null) {
                for (byte[] b: messageIdBytes) {
                    String[] args = new String(b).split(" ");
                    byte[] key = getKey(args[2]);
                    responses.add(pipeline.get(key));
                }
                pipeline.sync();
                for (Response<byte[]> response : responses) {
                    if (response.get() != null) {
                        BackplaneMessage backplaneMessage = (BackplaneMessage) SerializationUtils.deserialize(response.get());
                        messages.add(backplaneMessage);
                    } else {
                        logger.warn("failed to retrieve a message");
                    }
                }
            }

            Collections.sort(messages, new Comparator<BackplaneMessage>() {
                @Override
                public int compare(BackplaneMessage backplaneMessage, BackplaneMessage backplaneMessage1) {
                    return backplaneMessage.getIdValue().compareTo(backplaneMessage1.getIdValue());
                }
            });

            return messages;

        } finally {
            Redis.getInstance().releaseToPool(jedis);
        }
    }

    @Override
    public List<BackplaneMessage> retrieveMessagesByChannel(String channel) throws BackplaneServerException {

        Jedis jedis = null;

        try {

            jedis = Redis.getInstance().getJedis();

            List<BackplaneMessage> messages = new ArrayList<BackplaneMessage>();
            List<byte[]> messageIdBytes = jedis.lrange(getChannelKey(channel), 0, -1);

            Pipeline pipeline = jedis.pipelined();
            List<Response<byte[]>> responses = new ArrayList<Response<byte[]>>();

            if (messageIdBytes != null) {
                for (byte[] b: messageIdBytes) {
                    responses.add(pipeline.get(getKey(new String(b))));
                }
                pipeline.sync();
                for (Response<byte[]> response : responses) {
                    if (response.get() != null) {
                        BackplaneMessage backplaneMessage = (BackplaneMessage) SerializationUtils.deserialize(response.get());
                        messages.add(backplaneMessage);
                    } else {
                        logger.warn("failed to retrieve a message");
                    }
                }
            }

            Collections.sort(messages, new Comparator<BackplaneMessage>() {
                @Override
                public int compare(BackplaneMessage backplaneMessage, BackplaneMessage backplaneMessage1) {
                    return backplaneMessage.getIdValue().compareTo(backplaneMessage1.getIdValue());
                }
            });

            return messages;

        } finally {
            Redis.getInstance().releaseToPool(jedis);
        }

    }

    @Override
    public void deleteExpiredMessages() throws BackplaneServerException {

        Jedis jedis = null;

        try {

            logger.info("preparing to cleanup v2 messages");

            jedis = Redis.getInstance().getJedis();

            Set<byte[]> messageMetaBytes = jedis.zrangeByScore(V2_MESSAGES.getBytes(), 0, Double.MAX_VALUE);
            if (messageMetaBytes != null) {
                for (byte[] b : messageMetaBytes) {
                    String metaData = new String(b);
                    String[] segs = metaData.split(" ");
                    if (jedis.get(getKey(segs[2])) == null) {
                        // remove this key from indexes
                        logger.info("removing message " + segs[2]);
                        if (jedis.zrem(getBusKey(segs[0]), segs[2].getBytes()) != 1) {
                            logger.warn("could not remove message " + segs[2] + " from " + getBusKey(segs[0]));
                        }

                        if (jedis.lrem(getChannelKey(segs[1]), 0, segs[2].getBytes()) != 1) {
                            logger.warn("could not remove message " + segs[2] + " from " + getChannelKey(segs[1]));
                        }

                        if (jedis.zrem(V2_MESSAGES.getBytes(), metaData.getBytes()) != 1) {
                            logger.warn("could not remove message + " + segs[2] + " from " + V2_MESSAGES);
                        }
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

    @Override
    public BackplaneMessage get(String id) throws BackplaneServerException {
        byte[] messageBytes = Redis.getInstance().get(getKey(id));
        if (messageBytes != null) {
            return (BackplaneMessage) SerializationUtils.deserialize(messageBytes);
        }
        return null;
    }

    @Override
    public List<BackplaneMessage> getAll() throws BackplaneServerException {
        return retrieveMessagesNoScope(null);
    }

    @Override
    public void persist(BackplaneMessage obj) throws BackplaneServerException {
        // the messages will not be immediately available for reading until they
        // are inserted by the message processing thread.
        Redis.getInstance().rpush(V2_MESSAGE_QUEUE.getBytes(), SerializationUtils.serialize(obj));
    }

    @Override
    public void delete(String id) throws BackplaneServerException, TokenException {
        Jedis jedis = null;
        try {
            jedis = Redis.getInstance().getJedis();
            Date d = BackplaneMessage.getDateFromId(id);
            Set<String> sortedSetBytes = jedis.zrangeByScore(V2_MESSAGES, d.getTime(), d.getTime());
            byte[] bytes = jedis.get(getKey(id));

            if (!sortedSetBytes.isEmpty()) {
                Iterator it = sortedSetBytes.iterator();
                String key = (String) it.next();
                Transaction t = jedis.multi();

                Response<Long> del1 = t.zrem(V2_MESSAGES, key);
                String[] args = key.split(" ");
                Response<Long> del2 = t.lrem(getChannelKey(args[1]), 0, bytes);
                Response<Long> del3 = t.zrem(getBusKey(args[0]), args[2].getBytes());
                t.del(getKey(id));

                t.exec();

                if (del1.get() == 0) {
                    logger.warn("could not remove message " + id + " from " + V2_MESSAGES);
                }
                if (del2.get() == 0) {
                    logger.warn("could not remove message " + id + " from " + new String(getChannelKey(args[1])));
                }
                if (del3.get() == 0) {
                    logger.warn("could not remove message " + id + " from " + new String(getBusKey(args[0])));
                }
            }
        } finally {
            Redis.getInstance().releaseToPool(jedis);
        }
    }

    // PRIVATE

    private static final int MAX_MSGS_IN_FRAME = 25;

    private static final Logger logger = Logger.getLogger(RedisBackplaneMessageDAO.class);

    private void filterMessagesPerScope(List<BackplaneMessage> unfilteredMessages, Scope scope, MessagesResponse bpResponse) {
        // Filter and add to results
        List<BackplaneMessage> filteredMessages = new ArrayList<BackplaneMessage>();
        for (BackplaneMessage unfilteredMessage : unfilteredMessages) {
            if (scope.isMessageInScope(unfilteredMessage)) {
                if (filteredMessages.size() >= MAX_MSGS_IN_FRAME) {
                    bpResponse.moreMessages(true);
                    bpResponse.setLastMessageId(filteredMessages.get(filteredMessages.size() - 1).getIdValue());
                    break;
                }
                filteredMessages.add(unfilteredMessage);
            }
        }

        // update lastMessageId to point to last message in this unfiltered result
        if (unfilteredMessages.size() > 0 && !bpResponse.moreMessages()) {
            bpResponse.setLastMessageId(unfilteredMessages.get(unfilteredMessages.size() - 1).getIdValue());
        }

        bpResponse.addMessages(filteredMessages);
    }


}
