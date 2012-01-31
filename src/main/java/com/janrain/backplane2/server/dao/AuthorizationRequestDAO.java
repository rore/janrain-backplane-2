package com.janrain.backplane2.server.dao;

import com.janrain.backplane2.server.AuthorizationRequest;
import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;

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
}
