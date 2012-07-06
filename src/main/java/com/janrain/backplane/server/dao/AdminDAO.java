package com.janrain.backplane.server.dao;


import com.janrain.backplane.server.BackplaneServerException;
import com.janrain.backplane.server.User;
import com.janrain.redis.Redis;
import org.apache.commons.lang.NotImplementedException;
import org.apache.commons.lang.SerializationUtils;
import org.apache.log4j.Logger;

import java.util.List;

/**
 * @author Tom Raney
 */
public class AdminDAO extends DAO<User> {

    public static byte[] getAdminUserKey(String userId) {
        return ("v1_admin_" + userId).getBytes();
    }

    AdminDAO() {
        super();
    }

    @Override
    public void persist(User user) throws BackplaneServerException {
        byte[] key = getAdminUserKey(user.getIdValue());
        logger.info("writing key to redis: " + new String(key));
        Redis.getInstance().set(getAdminUserKey(user.getIdValue()), SerializationUtils.serialize(user));
    }

    @Override
    public void delete(String id) throws BackplaneServerException {
        byte[] key = getAdminUserKey(id);
        Redis.getInstance().del(key);
    }

    @Override
    public User get(String key) {
        byte[] bytes = Redis.getInstance().get(getAdminUserKey(key));
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

    private static final Logger logger = Logger.getLogger(AdminDAO.class);
}
