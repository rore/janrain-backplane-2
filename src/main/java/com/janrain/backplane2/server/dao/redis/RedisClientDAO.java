package com.janrain.backplane2.server.dao.redis;

import com.janrain.backplane2.server.BackplaneServerException;
import com.janrain.backplane2.server.config.Client;
import com.janrain.backplane2.server.dao.ClientDAO;
import com.janrain.oauth2.TokenException;
import com.janrain.redis.Redis;
import org.apache.commons.lang.NotImplementedException;
import org.apache.commons.lang.SerializationUtils;

import java.util.List;

/**
 * @author Tom Raney
 */
public class RedisClientDAO implements ClientDAO {

    public static byte[] getKey(String id) {
        return new String("v2_client_" + id).getBytes();
    }

    @Override
    public Client get(String id) throws BackplaneServerException {
        byte[] bytes = Redis.getInstance().get(getKey(id));
        if (bytes != null) {
            return (Client) SerializationUtils.deserialize(bytes);
        } else {
            return null;
        }
    }

    @Override
    public List<Client> getAll() throws BackplaneServerException {
        throw new NotImplementedException();
    }

    @Override
    public void persist(Client obj) throws BackplaneServerException {
        Redis.getInstance().set(getKey(obj.getIdValue()), SerializationUtils.serialize(obj));
    }

    @Override
    public void delete(String id) throws BackplaneServerException, TokenException {
        Redis.getInstance().del(getKey(id));
    }
}
