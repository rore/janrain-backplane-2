package com.janrain.backplane2.server.dao.simpledb;

import com.janrain.backplane2.server.BackplaneServerException;
import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.backplane2.server.config.User;
import com.janrain.backplane2.server.dao.AdminDAO;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import com.janrain.oauth2.TokenException;

import java.util.List;

/**
 * @author Tom Raney
 */
public class SimpleDBAdminDAO implements AdminDAO {

    public SimpleDBAdminDAO(SuperSimpleDB superSimpleDB, Backplane2Config bpConfig) {
        this.bpConfig = bpConfig;
        this.superSimpleDB = superSimpleDB;
    }

    @Override
    public User get(String id) throws BackplaneServerException {
        try {
            return superSimpleDB.retrieve(bpConfig.getTableName(Backplane2Config.SimpleDBTables.BP_ADMIN_AUTH), User.class, id);
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }

    @Override
    public List<User> getAll() throws BackplaneServerException {
        try {
            return superSimpleDB.retrieveAll(bpConfig.getTableName(Backplane2Config.SimpleDBTables.BP_ADMIN_AUTH), User.class);
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }

    @Override
    public void persist(User obj) throws BackplaneServerException {
        try {
            superSimpleDB.store(bpConfig.getTableName(Backplane2Config.SimpleDBTables.BP_ADMIN_AUTH), User.class, obj);
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }

    @Override
    public void delete(String id) throws BackplaneServerException, TokenException {
        try {
            superSimpleDB.delete(bpConfig.getTableName(Backplane2Config.SimpleDBTables.BP_ADMIN_AUTH), id);
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }

    private final SuperSimpleDB superSimpleDB;
    private final Backplane2Config bpConfig;

}
