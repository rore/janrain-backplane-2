package com.janrain.backplane2.server.dao.redis;

import com.janrain.backplane.server.dao.redis.RedisConfigDAO;
import com.janrain.backplane2.server.config.BusConfig2;
import com.janrain.backplane2.server.config.Client;
import com.janrain.backplane2.server.config.User;
import com.janrain.backplane2.server.dao.*;
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
        return busDao;
    }

    @Override
    public TokenDAO getTokenDao() {
        return tokenDao;
    }

    @Override
    public GrantDAO getGrantDao() {
        return grantDao;
    }

    @Override
    public BusOwnerDAO getBusOwnerDAO() {
        return busOwnerDao;
    }

    @Override
    public ClientDAO getClientDAO() {
        return clientDao;
    }

    @Override
    public BackplaneMessageDAO getBackplaneMessageDAO() {
        return messageDao;
    }

    @Override
    public AuthSessionDAO getAuthSessionDAO() {
        return authSessionDao;
    }

    @Override
    public AuthorizationRequestDAO getAuthorizationRequestDAO() {
        return authorizationRequestDao;
    }

    @Override
    public AuthorizationDecisionKeyDAO getAuthorizationDecisionKeyDAO() {
        return authorizationDecisionKeyDao;
    }

    @Override
    public RedisConfigDAO getConfigDAO() {
        return new RedisConfigDAO();
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
        return new RedisAdminDAO();
    }

    // - PRIVATE

    private static BusDAO busDao = new RedisBusDAO();
    private static TokenDAO tokenDao = new RedisTokenDAO();
    private static GrantDAO grantDao = new RedisGrantDAO(new RedisTokenDAO());
    private static BusOwnerDAO busOwnerDao = new RedisBusOwnerDAO();
    private static ClientDAO clientDao = new RedisClientDAO();
    private static BackplaneMessageDAO messageDao = new RedisBackplaneMessageDAO();
    private static AuthSessionDAO authSessionDao = new RedisAuthSessionDAO();
    private static AuthorizationRequestDAO authorizationRequestDao = new RedisAuthorizationRequestDAO();
    private static AuthorizationDecisionKeyDAO authorizationDecisionKeyDao = new RedisAuthorizationDecisionKeyDAO();

}
