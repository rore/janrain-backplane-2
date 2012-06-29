package com.janrain.backplane.server.dao;

import com.janrain.backplane.server.User;
import com.janrain.backplane.server.config.Backplane1Config;
import com.janrain.backplane.server.redis.Redis;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import org.apache.commons.lang.NotImplementedException;
import org.apache.commons.lang.SerializationUtils;
import org.apache.log4j.Logger;

/**
 * @author Tom Raney
 */
public class UserDAO extends DAO<User> {

    UserDAO() {
        super();
    }

    public static byte[] getUserKey(String userId) {
        return new String("v1_user_" + userId).getBytes();
    }

    public void persist(User user) {
        byte[] key = getUserKey(user.getIdValue());
        logger.info("writing key to redis: " + new String(key));
        Redis.getInstance().set(getUserKey(user.getIdValue()), SerializationUtils.serialize(user));
    }

    @Override
    public void delete(String id) throws SimpleDBException {
        throw new NotImplementedException();
    }

    public User get(String key) {
        byte[] bytes = Redis.getInstance().get(getUserKey(key));
        if (bytes != null) {
            return (User) SerializationUtils.deserialize(bytes);
        } else {
            return null;
        }
    }

    private static final Logger logger = Logger.getLogger(UserDAO.class);
}
