package com.janrain.backplane.server.dao;

import com.janrain.backplane.server.User;
import com.janrain.backplane.server.config.Backplane1Config;
import com.janrain.backplane.server.redis.Redis;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import org.apache.commons.lang.SerializationUtils;
import org.apache.log4j.Logger;

/**
 * @author Tom Raney
 */
public class AdminDAO extends DAO<User> {

    public static byte[] getAdminUserKey(String userId) {
        return new String("v1_admin_" + userId).getBytes();
    }

    AdminDAO() {
        super();
    }

    @Override
    public void persist(User user) throws SimpleDBException {
        byte[] key = getAdminUserKey(user.getIdValue());
        logger.info("writing key to redis: " + new String(key));
        Redis.getInstance().set(getAdminUserKey(user.getIdValue()), SerializationUtils.serialize(user));
    }

    @Override
    public void delete(String id) throws SimpleDBException {
        byte[] key = getAdminUserKey(id);
        Redis.getInstance().del(key);
    }

    private static final Logger logger = Logger.getLogger(AdminDAO.class);
}
