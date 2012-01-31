package com.janrain.backplane2.server.dao;

import com.janrain.backplane2.server.AuthorizationDecisionKey;
import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;

import static com.janrain.backplane2.server.config.Backplane2Config.SimpleDBTables.BP_AUTHORIZATION_DECISION_KEY;

/**
 * @author Johnny Bufu
 */
public class AuthorizationDecisionKeyDAO extends DAO {

    AuthorizationDecisionKeyDAO(SuperSimpleDB superSimpleDB, Backplane2Config bpConfig) {
        super(superSimpleDB, bpConfig);
    }

    public void persistAuthorizationDecisionKey(AuthorizationDecisionKey authorizationDecisionKey) throws SimpleDBException {
        superSimpleDB.store(bpConfig.getTableName(BP_AUTHORIZATION_DECISION_KEY), AuthorizationDecisionKey.class, authorizationDecisionKey);
    }

    public AuthorizationDecisionKey retrieveAuthorizationRequest(String key) throws SimpleDBException {
        return superSimpleDB.retrieve(bpConfig.getTableName(BP_AUTHORIZATION_DECISION_KEY), AuthorizationDecisionKey.class, key);
    }
}
