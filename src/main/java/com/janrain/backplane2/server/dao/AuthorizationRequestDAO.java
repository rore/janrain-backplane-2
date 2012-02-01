package com.janrain.backplane2.server.dao;

import com.janrain.backplane2.server.AuthorizationRequest;
import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import org.apache.log4j.Logger;

import java.util.Date;

import static com.janrain.backplane2.server.config.Backplane2Config.SimpleDBTables.BP_AUTHORIZATION_REQUEST;

/**
 * @author Johnny Bufu
 */
public class AuthorizationRequestDAO extends DAO {

    AuthorizationRequestDAO(SuperSimpleDB superSimpleDB, Backplane2Config bpConfig) {
        super(superSimpleDB, bpConfig);
    }

    public void persistAuthorizationRequest(AuthorizationRequest authorizationRequest) throws SimpleDBException {
        superSimpleDB.store(bpConfig.getTableName(BP_AUTHORIZATION_REQUEST), AuthorizationRequest.class, authorizationRequest);
    }

    public AuthorizationRequest retrieveAuthorizationRequest(String cookie) throws SimpleDBException {
        return superSimpleDB.retrieve(bpConfig.getTableName(BP_AUTHORIZATION_REQUEST), AuthorizationRequest.class, cookie);
    }

    public void deleteExpiredAuthorizationRequests() {
        try {
            logger.info("Backplane authorization requests cleanup task started.");
            String expiredClause = AuthorizationRequest.Field.EXPIRES.getFieldName() + " < '" + Backplane2Config.ISO8601.format(new Date(System.currentTimeMillis())) + "'";
            superSimpleDB.deleteWhere(bpConfig.getTableName(BP_AUTHORIZATION_REQUEST), expiredClause);
        } catch (Exception e) {
            // catch-all, else cleanup thread stops
            logger.error("Backplane authorization requests cleanup task error: " + e.getMessage(), e);
        } finally {
            logger.info("Backplane authorization requests cleanup task finished.");
        }
    }

    // - PRIVATE

    private static final Logger logger = Logger.getLogger(AuthorizationRequestDAO.class);

}
