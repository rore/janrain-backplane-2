package com.janrain.backplane2.server.dao;

import com.janrain.backplane2.server.config.AuthException;
import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.backplane2.server.config.User;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import com.janrain.crypto.HmacHashUtils;

/**
 * @author Tom Raney
 */
public class UserDAO extends DAO {

    UserDAO(SuperSimpleDB superSimpleDB, Backplane2Config bpConfig) {
        super(superSimpleDB, bpConfig);
    };

    public void persistUser(User user) throws SimpleDBException {
        superSimpleDB.store(bpConfig.getUserTableName(), User.class, user);
    }

    public User retrieveUser(String userId) throws SimpleDBException {
        return superSimpleDB.retrieve(bpConfig.getUserTableName(), User.class, userId);
    }

    public void deleteUser(String userId) throws SimpleDBException {
        superSimpleDB.delete(bpConfig.getUserTableName(), userId);
    }

}
