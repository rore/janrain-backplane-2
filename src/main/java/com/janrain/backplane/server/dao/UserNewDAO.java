package com.janrain.backplane.server.dao;

import com.janrain.backplane.server.config.Backplane1Config;
import com.janrain.backplane.server.config.UserNew;
import com.janrain.backplane.server.redis.Redis;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import org.apache.log4j.Logger;

/**
 * @author Tom Raney
 */
public class UserNewDAO extends NewDAO<UserNew> {

    UserNewDAO(SuperSimpleDB superSimpleDB, Backplane1Config bpConfig, com.janrain.backplane.server.dao.DaoFactory daoFactory) {
        super(superSimpleDB, bpConfig);
        this.daoFactory = daoFactory;
    }

    public static byte[] getUserKey(String userId) {
        return new String("v1_user_" + userId).getBytes();
    }

    public void persist(UserNew user) {
        byte[] key = getUserKey(user.getId());
        logger.info("writing key to redis: " + new String(key));
        Redis.getInstance().set(getUserKey(user.getId()), user.toBytes());
    }

    @Override
    public void delete(String id) throws SimpleDBException {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public UserNew get(String key) {
        return UserNew.fromBytes(Redis.getInstance().get(getUserKey(key)));
    }

    private final DaoFactory daoFactory;

    private static final Logger logger = Logger.getLogger(UserNewDAO.class);
}
