package com.janrain.backplane.server.dao.redis;

import com.janrain.backplane.server.BackplaneServerException;
import com.janrain.backplane.server.dao.DAO;
import com.janrain.backplane2.server.config.User;
import com.janrain.redis.Redis;
import org.apache.commons.lang.NotImplementedException;
import org.apache.commons.lang.SerializationUtils;
import org.apache.log4j.Logger;

import java.util.List;

/**
 * @author Tom Raney
 */
public class RedisUserDAO extends DAO<User> {

    public static byte[] getUserKey(String userId) {
        return ("v1_user_" + userId).getBytes();
    }

    @Override
    public void persist(User user) {
        byte[] key = getUserKey(user.getIdValue());
        logger.info("writing key to redis: " + new String(key));
        Redis.getInstance().set(getUserKey(user.getIdValue()), SerializationUtils.serialize(user));
    }

    @Override
    public void delete(String id) throws BackplaneServerException {
        throw new NotImplementedException();
    }

    @Override
    public User get(String key) {
        byte[] bytes = Redis.getInstance().get(getUserKey(key));
        if (bytes != null) {
            return (User) SerializationUtils.deserialize(bytes);
        } else {
            return null;
        }
    }

    @Override
    public List<User> getAll() throws BackplaneServerException {
        throw new NotImplementedException();
    }

    private static final Logger logger = Logger.getLogger(RedisUserDAO.class);
}
