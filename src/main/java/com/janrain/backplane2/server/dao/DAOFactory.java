package com.janrain.backplane2.server.dao;

import com.janrain.backplane.server.dao.redis.RedisConfigDAO;
import com.janrain.backplane2.server.dao.redis.RedisDAOFactory;

/**
 * @author Tom Raney
 */
public abstract class DAOFactory {

    // List of DAO types supported by the factory
    public static final int REDIS = 1;

    public abstract BusDAO getBusDao();
    public abstract TokenDAO getTokenDao();
    public abstract GrantDAO getGrantDao();
    public abstract BusOwnerDAO getBusOwnerDAO();
    public abstract ClientDAO getClientDAO();
    public abstract BackplaneMessageDAO getBackplaneMessageDAO();
    public abstract AuthSessionDAO getAuthSessionDAO();
    public abstract AuthorizationRequestDAO getAuthorizationRequestDAO();
    public abstract AuthorizationDecisionKeyDAO getAuthorizationDecisionKeyDAO();
    public abstract RedisConfigDAO getConfigDAO();
    public abstract DAO getDaoByObjectType(Class<?> obj);
    public abstract AdminDAO getAdminDAO();

    public static DAOFactory getDAOFactory(int factory) {
        switch (factory) {
            case REDIS:
                return new RedisDAOFactory();
            default:
                return null;
        }
    }
}
