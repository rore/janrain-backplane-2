package com.janrain.backplane.server.dao;

import com.janrain.backplane.server.BackplaneMessage;
import com.janrain.backplane.server.Scope;
import com.janrain.backplane.server.Token;
import com.janrain.backplane.server.config.BackplaneConfig;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;

import java.util.List;

/**
 * @author Tom Raney
 */
public class BackplaneMessageDAO extends DAO {

    BackplaneMessageDAO(SuperSimpleDB superSimpleDB, BackplaneConfig bpConfig) {
        super(superSimpleDB, bpConfig);
    };

    public void persistBackplaneMessage(BackplaneMessage message) throws SimpleDBException {
        superSimpleDB.store(bpConfig.getMessagesTableName(), BackplaneMessage.class, message);
    }

    public BackplaneMessage retrieveBackplaneMessage(String messageId) throws SimpleDBException {
        return superSimpleDB.retrieve(bpConfig.getMessagesTableName(), BackplaneMessage.class, messageId);
    }

    public List<BackplaneMessage> retrieveAllMesssagesPerScope(Scope scope) throws SimpleDBException {
        return superSimpleDB.retrieveWhere(bpConfig.getMessagesTableName(), BackplaneMessage.class, scope.buildQueryFromScope(), true);
    }

    public List<BackplaneMessage> retrieveAllMesssagesPerScopeSince(Scope scope, String sinceMessageId) throws SimpleDBException {
        return superSimpleDB.retrieveWhere(bpConfig.getMessagesTableName(), BackplaneMessage.class, scope.buildQueryFromScope(), true);
    }



}
