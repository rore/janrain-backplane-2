package com.janrain.backplane2.server.dao;

import com.janrain.backplane2.server.BackplaneMessage;
import com.janrain.backplane2.server.Scope;
import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;

import java.util.ArrayList;
import java.util.List;

/**
 * @author Tom Raney
 */
public class BackplaneMessageDAO extends DAO {

    BackplaneMessageDAO(SuperSimpleDB superSimpleDB, Backplane2Config bpConfig) {
        super(superSimpleDB, bpConfig);
    };

    public void persistBackplaneMessage(BackplaneMessage message) throws SimpleDBException {
        superSimpleDB.store(bpConfig.getMessagesTableName(), BackplaneMessage.class, message);
    }

    public BackplaneMessage retrieveBackplaneMessage(String messageId) throws SimpleDBException {
        return superSimpleDB.retrieve(bpConfig.getMessagesTableName(), BackplaneMessage.class, messageId);
    }

    public List<BackplaneMessage> retrieveAllMesssagesPerScope(Scope scope, String sinceMessageId) throws SimpleDBException {

        ArrayList<BackplaneMessage> messages = new ArrayList<BackplaneMessage>();

        // If the scope is complex, the risk is that we over-run SDB's query size restrictions.
        // So, here we break the query into chunks to run against SDB and build the result set
        // up incrementally.

        List<String> queries = scope.buildQueriesFromScope();

        for (String query : queries) {

            assert(StringUtils.isNotEmpty(query));

            if (StringUtils.isNotEmpty(sinceMessageId)) {
                query += " AND id > '" + sinceMessageId + "'";
            }

            logger.info("message query => " + query);

            messages.addAll(superSimpleDB.retrieveWhere(bpConfig.getMessagesTableName(), BackplaneMessage.class, query, true));
        }

        return messages;
    }

    private static final Logger logger = Logger.getLogger(BackplaneMessageDAO.class);

}
