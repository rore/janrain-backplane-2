package com.janrain.backplane2.server.dao;

import com.janrain.backplane2.server.AuthSession;
import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;

/**
 * @author Johnny Bufu
 */
public class AuthSessionDAO extends DAO {

    AuthSessionDAO(SuperSimpleDB superSimpleDB, Backplane2Config bpConfig) {
        super(superSimpleDB, bpConfig);
    }

    public void persistAuthSession(AuthSession authSession) throws SimpleDBException {
        superSimpleDB.store(bpConfig.getAuthSessionTableName(), AuthSession.class, authSession);
    }

    public AuthSession retrieveAuthSession(String cookie) throws SimpleDBException {
        return superSimpleDB.retrieve(bpConfig.getAuthSessionTableName(), AuthSession.class, cookie);
    }
}
