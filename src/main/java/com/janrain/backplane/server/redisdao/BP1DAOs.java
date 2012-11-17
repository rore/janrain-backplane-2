package com.janrain.backplane.server.redisdao;

import com.janrain.backplane.dao.DAO;
import com.janrain.backplane.dao.redis.RedisUserDAO;
import com.janrain.backplane.server.BackplaneMessage;
import com.janrain.backplane.server.BusConfig1;
import com.janrain.backplane.common.User;

/**
 * @author Johnny Bufu
 */
public class BP1DAOs {

    // - PUBLIC

    public static DAO<BusConfig1> getBusDao() {
        return busDao;
    }

    public static DAO<User> getUserDao() {
        return userDao;
    }

    public static BP1MessageDao getMessageDao() {
        return messageDao;
    }

    public static DAO getDaoByObjectType(Class<?> obj) {
        if (User.class.isAssignableFrom(obj)) {
            return getUserDao();
        } else if (BusConfig1.class.isAssignableFrom(obj)) {
            return getBusDao();
        } else if (BackplaneMessage.class.isAssignableFrom(obj)) {
            return getMessageDao();
        }

        return null;
    }

    // - PRIVATE

    private BP1DAOs() {}

    private static final DAO<BusConfig1> busDao = new RedisBusConfig1DAO();
    private static final DAO<User> userDao = new RedisUserDAO();
    private static final BP1MessageDao messageDao = new RedisBackplaneMessageDAO();

}
