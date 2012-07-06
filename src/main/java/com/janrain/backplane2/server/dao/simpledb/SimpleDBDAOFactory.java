package com.janrain.backplane2.server.dao.simpledb;

import com.janrain.backplane.server.dao.ConfigDAO;
import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.backplane2.server.config.BusConfig2;
import com.janrain.backplane2.server.config.Client;
import com.janrain.backplane2.server.config.User;
import com.janrain.backplane2.server.dao.*;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import org.springframework.context.annotation.Scope;

import javax.inject.Inject;

/**
 * @author Tom Raney
 */
//@Service(value="simpleDBDaoFactory")
@Scope(value="singleton")
public class SimpleDBDAOFactory extends DAOFactory {

    @Override
    public BusDAO getBusDao() {
        return new SimpleDBBusDAO(superSimpleDB, bpConfig, this);
    }

    @Override
    public TokenDAO getTokenDao() {
        return new SimpleDBTokenDAO(superSimpleDB, bpConfig, this);
    }

    @Override
    public GrantDAO getGrantDao() {
        return new SimpleDBGrantDAO(superSimpleDB, bpConfig, this);
    }

    @Override
    public BusOwnerDAO getBusOwnerDAO() {
        return new SimpleDBBusOwnerDAO(superSimpleDB, bpConfig, this);
    }

    @Override
    public ClientDAO getClientDAO() {
        return new SimpleDBClientDAO(superSimpleDB, bpConfig, this);
    }

    @Override
    public BackplaneMessageDAO getBackplaneMessageDAO() {
        return new SimpleDBBackplaneMessageDAO(superSimpleDB, bpConfig, this);
    }

    @Override
    public AuthSessionDAO getAuthSessionDAO() {
        return new SimpleDBAuthSessionDAO(superSimpleDB, bpConfig);
    }

    @Override
    public AuthorizationRequestDAO getAuthorizationRequestDAO() {
        return new SimpleDBAuthorizationRequestDAO(superSimpleDB, bpConfig);
    }

    @Override
    public AuthorizationDecisionKeyDAO getAuthorizationDecisionKeyDAO() {
        return new SimpleDBAuthorizationDecisionKeyDAO(superSimpleDB, bpConfig);
    }

    @Override
    public ConfigDAO getConfigDAO() {
        return new ConfigDAO();
    }

    @Override
    public DAO getDaoByObjectType(Class<?> obj) {
        if (Client.class.isAssignableFrom(obj)) {
            return getClientDAO();
        } else if (User.class.isAssignableFrom(obj)) {
            return getBusOwnerDAO();
        } else if (BusConfig2.class.isAssignableFrom(obj)) {
            return getBusDao();
        }

        return null;
    }

    @Override
    public AdminDAO getAdminDAO() {
        return new SimpleDBAdminDAO(superSimpleDB, bpConfig);
    }


    // PRIVATE

    @Inject
    private SuperSimpleDB superSimpleDB;

    @Inject
    private Backplane2Config bpConfig;
}
