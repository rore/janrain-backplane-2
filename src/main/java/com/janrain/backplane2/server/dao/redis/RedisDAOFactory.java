package com.janrain.backplane2.server.dao.redis;

import com.janrain.backplane.server.dao.ConfigDAO;
import com.janrain.backplane2.server.dao.*;
import org.apache.commons.lang.NotImplementedException;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Service;

/**
 * @author Tom Raney
 */

@Service(value="redisDaoFactory")
@Scope(value="singleton")
public class RedisDAOFactory extends DAOFactory {

    @Override
    public BusDAO getBusDao() {
        return new RedisBusDAO();
    }

    @Override
    public TokenDAO getTokenDao() {
        return new RedisTokenDAO();
    }

    @Override
    public GrantDAO getGrantDao() {
        return new RedisGrantDAO();
    }

    @Override
    public BusOwnerDAO getBusOwnerDAO() {
        return new RedisBusOwnerDAO();
    }

    @Override
    public ClientDAO getClientDAO() {
        return new RedisClientDAO();
    }

    @Override
    public BackplaneMessageDAO getBackplaneMessageDAO() {
        return new RedisBackplaneMessageDAO();
    }

    @Override
    public AuthSessionDAO getAuthSessionDAO() {
        return new RedisAuthSessionDAO();
    }

    @Override
    public AuthorizationRequestDAO getAuthorizationRequestDAO() {
        return new RedisAuthorizationRequestDAO();
    }

    @Override
    public AuthorizationDecisionKeyDAO getAuthorizationDecisionKeyDAO() {
        return new RedisAuthorizationDecisionKeyDAO();
    }

    @Override
    public ConfigDAO getConfigDAO() {
        return new ConfigDAO();
    }

    @Override
    public DAO getDaoByObjectType(Class<?> obj) {
        throw new NotImplementedException();
    }

    @Override
    public AdminDAO getAdminDAO() {
        return new RedisAdminDAO();
    }
}
