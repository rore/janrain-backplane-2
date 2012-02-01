package com.janrain.backplane2.server.dao;

import com.janrain.backplane2.server.AuthorizationDecisionKey;
import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import org.apache.log4j.Logger;

import java.util.Date;

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

    public void deleteExpiredAuthorizationDecisionKeys() {
        try {
            logger.info("Backplane authorization decision keys cleanup task started.");
            String expiredClause = AuthorizationDecisionKey.Field.EXPIRES.getFieldName() + " < '" + Backplane2Config.ISO8601.format(new Date(System.currentTimeMillis())) + "'";
            superSimpleDB.deleteWhere(bpConfig.getTableName(BP_AUTHORIZATION_DECISION_KEY), expiredClause);
        } catch (Exception e) {
            // catch-all, else cleanup thread stops
            logger.error("Backplane authorization decision keys cleanup task error: " + e.getMessage(), e);
        } finally {
            logger.info("Backplane authorization decision keys cleanup task finished.");
        }
    }

    // - PRIVATE

    private static final Logger logger = Logger.getLogger(AuthorizationRequestDAO.class);


}
