package com.janrain.backplane2.server.dao.simpledb;

import com.janrain.backplane.server.dao.redis.RedisConfigDAO;
import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.backplane2.server.config.BusConfig2;
import com.janrain.backplane2.server.config.Client;
import com.janrain.backplane2.server.config.User;
import com.janrain.backplane2.server.dao.*;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import org.apache.log4j.Logger;
import org.springframework.context.annotation.Scope;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

/**
 * @author Tom Raney
 */
//@Service(value="simpleDBDaoFactory")
@Scope(value="singleton")
public class SimpleDBDAOFactory extends DAOFactory {

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
        return new SimpleDBAdminDAO(superSimpleDB, bpConfig);
    }

    // PRIVATE

    private static final Logger logger = Logger.getLogger(SimpleDBDAOFactory.class);

    @Inject
    private SuperSimpleDB superSimpleDB;

    @Inject
    private Backplane2Config bpConfig;

    private static final Object initLock = new Object();
    private static boolean initialized = false;

    private static BusDAO busDao;
    private static TokenDAO tokenDao;
    private static GrantDAO grantDao;
    private static BusOwnerDAO busOwnerDao;
    private static ClientDAO clientDao;
    private static BackplaneMessageDAO messageDao;
    private static AuthSessionDAO authSessionDao;
    private static AuthorizationRequestDAO authorizationRequestDao;
    private static AuthorizationDecisionKeyDAO authorizationDecisionKeyDao;

    @SuppressWarnings({"AssignmentToStaticFieldFromInstanceMethod", "UnusedDeclaration"})
    @PostConstruct
    private void init() {
        synchronized (initLock) {
            if (initialized) {
                logger.warn("attempt to initialize singleton more than once");
                return;
            }
            busDao = new SimpleDBBusDAO(superSimpleDB, bpConfig, this);
            tokenDao = new SimpleDBTokenDAO(superSimpleDB, bpConfig);
            grantDao = new SimpleDBGrantDAO(superSimpleDB, bpConfig, this);
            busOwnerDao = new SimpleDBBusOwnerDAO(superSimpleDB, bpConfig, this);
            clientDao = new SimpleDBClientDAO(superSimpleDB, bpConfig, this);
            messageDao = new SimpleDBBackplaneMessageDAO(superSimpleDB, bpConfig, this);
            authSessionDao = new SimpleDBAuthSessionDAO(superSimpleDB, bpConfig);
            authorizationRequestDao = new SimpleDBAuthorizationRequestDAO(superSimpleDB, bpConfig);
            authorizationDecisionKeyDao = new SimpleDBAuthorizationDecisionKeyDAO(superSimpleDB, bpConfig);
            initialized = true;
        }
    }
}
