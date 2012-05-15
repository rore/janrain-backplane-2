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

package com.janrain.backplane2.server.dao;

import com.janrain.backplane2.server.BackplaneMessage;
import com.janrain.backplane2.server.Scope;
import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.backplane2.server.config.BusConfig2;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import com.janrain.commons.util.Pair;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;

import java.util.ArrayList;
import java.util.Date;
import java.util.List;

import static com.janrain.backplane2.server.BackplaneMessage.Field.*;
import static com.janrain.backplane2.server.config.Backplane2Config.SimpleDBTables.BP_MESSAGES;
import static com.janrain.backplane2.server.config.BusConfig2.Field.*;

/**
 * @author Tom Raney
 */
public class BackplaneMessageDAO extends DAO<BackplaneMessage> {

    public static final int MAX_MSGS_IN_FRAME = 25;

    BackplaneMessageDAO(SuperSimpleDB superSimpleDB, Backplane2Config bpConfig, DaoFactory daoFactory) {
        super(superSimpleDB, bpConfig);
        this.daoFactory = daoFactory;
    }

    @Override
    public void persist(BackplaneMessage message) throws SimpleDBException {
        superSimpleDB.store(bpConfig.getTableName(BP_MESSAGES), BackplaneMessage.class, message);
    }

    public BackplaneMessage getLatestMessage() throws SimpleDBException {
        String query = "id IS NOT NULL ORDER BY id DESC LIMIT 1";
        Pair<List<BackplaneMessage>, Boolean> messages=
                superSimpleDB.retrieveSomeWhere(bpConfig.getTableName(BP_MESSAGES), BackplaneMessage.class, query);
        if (messages.getLeft().size() > 0) {
            return messages.getLeft().get(0);
        } else {
            return null;
        }
    }

    @Override
    public void delete(String id) throws SimpleDBException {
        superSimpleDB.delete(bpConfig.getTableName(BP_MESSAGES), id);
    }

    public BackplaneMessage retrieveBackplaneMessage(String messageId) throws SimpleDBException {
        return superSimpleDB.retrieve(bpConfig.getTableName(BP_MESSAGES), BackplaneMessage.class, messageId);
    }

    public boolean isChannelFull(String channel) throws SimpleDBException {
        String whereClause = " channel='" + channel + "'";
        long count = superSimpleDB.retrieveCount(bpConfig.getTableName(BP_MESSAGES), whereClause);
        logger.debug(count + " >= " + bpConfig.getDefaultMaxMessageLimit());
        return count >= bpConfig.getDefaultMaxMessageLimit();
    }

    public boolean canTake(String channel, int msgPostCount) throws SimpleDBException {
        String whereClause = " channel='" + channel + "'";
        long count = superSimpleDB.retrieveCount(bpConfig.getTableName(BP_MESSAGES), whereClause);
        logger.debug(count + " >= " + bpConfig.getDefaultMaxMessageLimit());
        return count + msgPostCount < bpConfig.getDefaultMaxMessageLimit();
    }

    /**
     * Retrieve all messages by per scope.  Guaranteed to delivery results in order of
     * message ID.
     * @param scope
     * @param sinceMessageId
     * @param limit  -1 will set limit = MAX_MSGS_IN_FRAME +1
     * @return returns a maximum of MAX_MSGS_IN_FRAME + 1
     * @throws SimpleDBException
     */

    public List<BackplaneMessage> retrieveAllMesssagesPerScope(Scope scope, String sinceMessageId, int limit, String anonymousTokenBus) throws SimpleDBException {

        List<BackplaneMessage> filteredMessages = new ArrayList<BackplaneMessage>();

        // In order to satisfy the ordering by message ID and not omit records, the query must be made against
        // SDB purely on the message ID.  We will then do scope filtering before adding
        // the message to the query results.

        if (limit < 0 || limit > MAX_MSGS_IN_FRAME) {
            // add one to the results, to properly show that more results may remain, if they do
            limit = MAX_MSGS_IN_FRAME+1;
        }

        Pair<List<BackplaneMessage>, Boolean> unfilteredMessages;

        do {

            StringBuilder query = new StringBuilder();
            // SDB's default query limit is 100, but we want to be as efficient as possible and pull as many records
            // as SDB will provide for processing to minimize network lag.  It will return a maximum of 1 meg
            // per request regardless.
            int queryLimit = 2500;

            // Optimization if only one channel is listed in scope
            if (scope.getChannelsInScope().size() == 1) {
                query.append(" channel='").append(scope.getChannelsInScope().get(0)).append("' AND");
                queryLimit = limit;
            }
            
            if (StringUtils.isNotEmpty(anonymousTokenBus)) {
                // anonymous tokens generate a channel-only scope
                // make sure we only return messages that belong to the bus that channel is bound to (through the anonymous token)
                // in the (extremely) unlikely case the same channel name is generated for two buses
                query.append(" bus='").append(anonymousTokenBus).append("' AND");
            }

            if (StringUtils.isNotEmpty(sinceMessageId)) {
                query.append(" id > '" ).append(sinceMessageId).append("' AND");
            }

            query.append(" id IS NOT NULL ORDER BY id LIMIT ").append(queryLimit);

            // We don't want to use SDB's tokenizing mechanism to continually loop as it does this
            // without read consistency
            unfilteredMessages = superSimpleDB.retrieveSomeWhere(bpConfig.getTableName(BP_MESSAGES), BackplaneMessage.class, query.toString());

            // Filter and add to results
            for (BackplaneMessage unfilteredMessage : unfilteredMessages.getLeft()) {
                if (scope.isMessageInScope(unfilteredMessage) && filteredMessages.size() < limit) {
                    filteredMessages.add(unfilteredMessage);
                }
            }

            // update sinceMessageId to point to last message in this unfiltered result
            if (unfilteredMessages.getLeft().size() > 0) {
                sinceMessageId = unfilteredMessages.getLeft().get(unfilteredMessages.getLeft().size()-1).getIdValue();
            }

        } while (unfilteredMessages.getLeft().size() > 0 && filteredMessages.size() < limit);

        return filteredMessages;
    }
    
    public void deleteExpiredMessages() {
        try {
            logger.info("Backplane message cleanup task started.");
            String messagesTable = bpConfig.getTableName(BP_MESSAGES);
            for(BusConfig2 busConfig : daoFactory.getBusDao().retrieveBuses()) {
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
    
    // - PRIVATE

    private static final Logger logger = Logger.getLogger(BackplaneMessageDAO.class);

    private DaoFactory daoFactory;

    private String getExpiredMessagesClause(String busId, boolean sticky, String retentionTimeSeconds) {
        return BUS.getFieldName() + " = '" + busId + "' AND " +
                // "is (not) null" is low-performance on simpledb apparently
                // http://practicalcloudcomputing.com/post/722621724/simpledb-essentials-for-high-performance-users-part-2
                STICKY.getFieldName() + " = '" + Boolean.toString(sticky) + "' AND " +
                ID.getFieldName() + " < '" +
                Backplane2Config.ISO8601.format(new Date(System.currentTimeMillis() - Long.valueOf(retentionTimeSeconds) * 1000))
                + "'";
    }
}
