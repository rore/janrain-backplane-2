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

package com.janrain.backplane2.server.dao.simpledb;

import com.janrain.backplane2.server.*;
import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.backplane2.server.config.BusConfig2;
import com.janrain.backplane2.server.dao.BackplaneMessageDAO;
import com.janrain.backplane2.server.dao.DAOFactory;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import com.janrain.commons.util.InitSystemProps;
import com.janrain.commons.util.Pair;
import com.janrain.oauth2.TokenException;
import com.yammer.metrics.Metrics;
import com.yammer.metrics.core.Histogram;
import org.apache.commons.lang.NotImplementedException;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import javax.servlet.http.HttpServletResponse;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.TimeUnit;

import static com.janrain.backplane2.server.BackplaneMessage.Field.*;
import static com.janrain.backplane2.server.config.Backplane2Config.SimpleDBTables.BP_MESSAGES;
import static com.janrain.backplane2.server.config.BusConfig2.Field.*;

/**
 * @author Tom Raney, Johnny Bufu
 */
public class SimpleDBBackplaneMessageDAO implements BackplaneMessageDAO {


    // - PUBLIC

    String getTableName() {
        return System.getProperty(InitSystemProps.AWS_INSTANCE_ID) + "_v2_messages";
    }

    @Override
    public BackplaneMessage get(String id) throws BackplaneServerException {
        throw new NotImplementedException();
    }

    @Override
    public List<BackplaneMessage> getAll() throws BackplaneServerException {
        try {
            return superSimpleDB.retrieveAll(getTableName(), BackplaneMessage.class);
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }

    @Override
    public void persist(final BackplaneMessage message) throws BackplaneServerException {
        try {
            v2postTimer.time(new Callable<Object>() {
                @Override
                public Object call() throws Exception {
                    superSimpleDB.store(getTableName(), BackplaneMessage.class, message, true);
                    return null;
                }
            });
        } catch (Exception e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }

    @Override
    public BackplaneMessage getLatestMessage() throws BackplaneServerException {
        String query = "id IS NOT NULL ORDER BY id DESC LIMIT 1";
        Pair<List<BackplaneMessage>, Boolean> messages;
        try {
            messages = superSimpleDB.retrieveSomeWhere(getTableName(), BackplaneMessage.class, query);
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage());
        }
        if (messages.getLeft().size() > 0) {
            return messages.getLeft().get(0);
        } else {
            return null;
        }
    }

    @Override
    public void delete(String id) throws BackplaneServerException {
        try {
            superSimpleDB.delete(getTableName(), id);
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }

    @Override
    public @NotNull BackplaneMessage retrieveBackplaneMessage(@NotNull final String messageId, @NotNull Token token) throws BackplaneServerException, TokenException {
        BackplaneMessage message;
        try {
            message = v2singleGetTimer.time(new Callable<BackplaneMessage>() {
                @Override
                public BackplaneMessage call() throws Exception {
                    return superSimpleDB.retrieve(getTableName(), BackplaneMessage.class, messageId);
                }
            });
        } catch (Exception e) {
            throw new BackplaneServerException(e.getMessage());
        }

        if ( message == null || ! token.getScope().isMessageInScope(message)) {
            // don't disclose that the messageId exists if not in scope
            throw new TokenException("Message id '" + messageId + "' not found", HttpServletResponse.SC_NOT_FOUND);
        } else {
            return message;
        }
    }

    @Override
    public long getMessageCount(String channel) throws BackplaneServerException {
        try {
            return superSimpleDB.retrieveCount(getTableName(), " channel='" + channel + "'");
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }

    @Override
    public long countMessages() throws BackplaneServerException {
        try {
            Long messageCount = superSimpleDB.retrieveCount(getTableName(), null);
            v2messageCount.update(messageCount);
            return messageCount;
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }

    /**
     * Retrieve all messages by per scope in the provided bpResponse object.
     * Guaranteed to delivery results in order of message ID.
     *
     * @param bpResponse backplane message response
     * @param token access token to be used for message retrieval
     */
    @Override
    public void retrieveMessagesPerScope(@NotNull final MessagesResponse bpResponse, @NotNull final Token token) throws BackplaneServerException {
        final Scope scope = token.getScope();
        try {
            v2multiGetTimer.time(new Callable<Object>() {
                @Override
                public Object call() throws Exception {
                    String query = buildMessageSelectQueryClause(scope);
                    Pair<List<BackplaneMessage>, Boolean> unfilteredMessages;
                    do {
                        // We don't want to use SDB's paginating mechanism and retrieve all available, unfiltered messages
                        unfilteredMessages = superSimpleDB.retrieveSomeWhere(bpConfig.getTableName(BP_MESSAGES), BackplaneMessage.class,
                                ((query.length() > 0) ? query + " AND " : "") +
                                " id > '" + bpResponse.getLastMessageId() + "' ORDER BY id LIMIT " + SuperSimpleDB.MAX_SELECT_PAGINATION_LIMIT);

                        filterMessagesPerScope(unfilteredMessages.getLeft(), scope, bpResponse);

                    } while (unfilteredMessages.getLeft().size() > 0 && !bpResponse.moreMessages());

                    return null;
                }
            });
        } catch (Exception e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }

    @Override
    public List<BackplaneMessage> retrieveMessagesNoScope(String sinceIso8601timestamp) throws BackplaneServerException {

        try {

            List<BackplaneMessage> messages = new ArrayList<BackplaneMessage>();
            Pair<List<BackplaneMessage>, Boolean> newMessages;
            String since = StringUtils.isEmpty(sinceIso8601timestamp) ? "" : sinceIso8601timestamp;
            while(true) {
                // We don't want to use SDB's paginating mechanism and retrieve all available, unfiltered messages
                newMessages = superSimpleDB.retrieveSomeWhere(bpConfig.getTableName(BP_MESSAGES), BackplaneMessage.class,
                        " id > '" + since + "' ORDER BY id LIMIT " + SuperSimpleDB.MAX_SELECT_PAGINATION_LIMIT);
                if (newMessages.getLeft().isEmpty()) {
                    break;
                } else {
                    messages.addAll(newMessages.getLeft());
                    since = newMessages.getLeft().get(0).getIdValue();
                    if (messages.size() >= MAX_MSGS_IN_FRAME) break;
                }
            }
            return messages;
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }

    @Override
    public List<BackplaneMessage> retrieveMessagesByChannel(String channel) throws BackplaneServerException {
        try {
            return superSimpleDB.retrieveWhere(bpConfig.getTableName(BP_MESSAGES), BackplaneMessage.class, "channel='" + channel + "'", true);
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }

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

    @Override
    public void deleteExpiredMessages() {
        try {
            logger.info("Backplane message cleanup task started.");
            String messagesTable = bpConfig.getTableName(BP_MESSAGES);
            for(BusConfig2 busConfig : daoFactory.getBusDao().getAll()) {
                try {
                    // non-sticky
                    superSimpleDB.deleteWhere(messagesTable, getExpiredMessagesClause(busConfig.get(BUS_NAME), false, busConfig.get(RETENTION_TIME_SECONDS)));
                    // sticky
                    superSimpleDB.deleteWhere(messagesTable, getExpiredMessagesClause(busConfig.get(BUS_NAME), true, busConfig.get(RETENTION_STICKY_TIME_SECONDS)));

                } catch (SimpleDBException sdbe) {
                    logger.error("Error cleaning up expired messages on bus "  + busConfig.get(BUS_NAME) + ", " + sdbe.getMessage(), sdbe);
                }
            }

        } catch (Exception e) {
            // catch-all, else cleanup thread stops
            logger.error("Backplane messages cleanup task error: " + e.getMessage(), e);
        } finally {
            logger.info("Backplane messages cleanup task finished.");
        }

    }

    // - PACKAGE

    SimpleDBBackplaneMessageDAO(SuperSimpleDB superSimpleDB, Backplane2Config bpConfig, DAOFactory daoFactory) {
        this.superSimpleDB = superSimpleDB;
        this.daoFactory = daoFactory;
        this.bpConfig = bpConfig;
    }

    // - PRIVATE

    private static final Logger logger = Logger.getLogger(SimpleDBBackplaneMessageDAO.class);

    private static final int MAX_MSGS_IN_FRAME = 25;

    private SuperSimpleDB superSimpleDB;
    private DAOFactory daoFactory;
    private Backplane2Config bpConfig;

    private final com.yammer.metrics.core.Timer v2postTimer = Metrics.newTimer(SimpleDBBackplaneMessageDAO.class, "v2_sdb_post_message", TimeUnit.MILLISECONDS, TimeUnit.MINUTES);
    private final com.yammer.metrics.core.Timer v2singleGetTimer = Metrics.newTimer(SimpleDBBackplaneMessageDAO.class, "v2_sdb_get_message", TimeUnit.MILLISECONDS, TimeUnit.MINUTES);
    private final com.yammer.metrics.core.Timer v2multiGetTimer = Metrics.newTimer(SimpleDBBackplaneMessageDAO.class, "v2_sdb_get_messages", TimeUnit.MILLISECONDS, TimeUnit.MINUTES);
    private final com.yammer.metrics.core.Timer v2channelCountTimer = Metrics.newTimer(SimpleDBBackplaneMessageDAO.class, "v2_sdb_channel_count", TimeUnit.MILLISECONDS, TimeUnit.MINUTES);
    private final Histogram v2messageCount = Metrics.newHistogram(SimpleDBBackplaneMessageDAO.class, "v2_sdb_message_count");

    private String getExpiredMessagesClause(String busId, boolean sticky, String retentionTimeSeconds) {
        return BUS.getFieldName() + " = '" + busId + "' AND " +
                // "is (not) null" is low-performance on simpledb apparently
                // http://practicalcloudcomputing.com/post/722621724/simpledb-essentials-for-high-performance-users-part-2
                STICKY.getFieldName() + " = '" + Boolean.toString(sticky) + "' AND " +
                ID.getFieldName() + " < '" +
                Backplane2Config.ISO8601.get().format(new Date(System.currentTimeMillis() - Long.valueOf(retentionTimeSeconds) * 1000))
                + "'";
    }

    private static String buildMessageSelectQueryClause(Scope scope) {
        StringBuilder query = new StringBuilder();
        if (scope != null) {
            for(BackplaneMessage.Field scopeKey : scope.getScopeMap().keySet()) {
                Set<String> scopeValues = scope.getScopeFieldValues(scopeKey);
                if (scopeValues != null && scopeValues.size() == 1) {
                    if (query.length() > 0) query.append(" AND ");
                    query.append(scopeKey.getFieldName()).append("='").append(scopeValues.iterator().next()).append("'");
                }
            }
        }
        return query.toString();
    }
}
