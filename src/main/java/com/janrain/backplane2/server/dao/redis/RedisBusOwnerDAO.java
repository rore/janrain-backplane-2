package com.janrain.backplane2.server.dao.redis;

import com.janrain.backplane2.server.BackplaneServerException;
import com.janrain.backplane2.server.config.User;
import com.janrain.backplane2.server.dao.BusOwnerDAO;
import com.janrain.oauth2.TokenException;
import com.janrain.redis.Redis;
import org.apache.commons.lang.NotImplementedException;
import org.apache.commons.lang.SerializationUtils;

import java.util.List;

/**
 * @author Tom Raney
 */
public class RedisBusOwnerDAO implements BusOwnerDAO {

    public static byte[] getKey(String id) {
        return new String("v2_bus_owner_" + id).getBytes();
    }

    @Override
    public User get(String id) throws BackplaneServerException {
        byte[] bytes = Redis.getInstance().get(getKey(id));
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

    @Override
    public void persist(User obj) throws BackplaneServerException {
        Redis.getInstance().set(getKey(obj.getIdValue()), SerializationUtils.serialize(obj));
    }

    @Override
    public void delete(String id) throws BackplaneServerException, TokenException {
        Redis.getInstance().del(getKey(id));
    }
}
