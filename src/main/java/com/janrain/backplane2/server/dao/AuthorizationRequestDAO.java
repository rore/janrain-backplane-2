package com.janrain.backplane2.server.dao;

import com.janrain.backplane2.server.AuthorizationRequest;
import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;

/**
 * @author Johnny Bufu
 */
public class AuthorizationRequestDAO extends DAO {

    AuthorizationRequestDAO(SuperSimpleDB superSimpleDB, Backplane2Config bpConfig) {
        super(superSimpleDB, bpConfig);
    }

    public void persistAuthorizationRequest(AuthorizationRequest authorizationRequest) throws SimpleDBException {
        superSimpleDB.store(bpConfig.getAuthorizationRequestTableName(), AuthorizationRequest.class, authorizationRequest);
    }

    public AuthorizationRequest retrieveAuthorizationRequest(String cookie) throws SimpleDBException {
        return superSimpleDB.retrieve(bpConfig.getAuthorizationRequestTableName(), AuthorizationRequest.class, cookie);
    }
}
