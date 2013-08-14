package com.janrain.backplane.server.redisdao;

import com.janrain.backplane.dao.DAO;
import com.janrain.backplane.dao.redis.RedisUserDAO;
import com.janrain.backplane.server.BusConfig1;
import com.janrain.backplane2.server.config.User;

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

    // - PRIVATE

    private BP1DAOs() {}

    private static final DAO<BusConfig1> busDao = new RedisBusConfig1DAO();
    private static final DAO<User> userDao = new RedisUserDAO();
    private static final BP1MessageDao messageDao = new RedisBackplaneMessageDAO();

}
