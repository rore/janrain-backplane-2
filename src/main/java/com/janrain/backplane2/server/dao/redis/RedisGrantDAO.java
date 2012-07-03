package com.janrain.backplane2.server.dao.redis;

import com.janrain.backplane2.server.BackplaneMessage;
import com.janrain.backplane2.server.BackplaneServerException;
import com.janrain.backplane2.server.Grant;
import com.janrain.backplane2.server.Scope;
import com.janrain.backplane2.server.dao.GrantDAO;
import com.janrain.oauth2.TokenException;
import com.janrain.redis.Redis;
import org.apache.commons.lang.NotImplementedException;
import org.apache.commons.lang.SerializationUtils;
import org.apache.zookeeper.server.util.SerializeUtils;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;
import redis.clients.jedis.Jedis;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.Set;

/**
 * @author Tom Raney
 */
public class RedisGrantDAO implements GrantDAO {

    public static byte[] getKey(String id) {
        return new String("v2_grant_" + id).getBytes();
    }


    @Override
    public List<Grant> getByClientId(String clientId) throws BackplaneServerException {
        List<Grant> grants = getAll();
        List<Grant> filtered = new ArrayList<Grant>();
        for (Grant grant: grants) {
            if (clientId.equals(grant.get(Grant.GrantField.ISSUED_TO_CLIENT_ID)) &&
                    (grant.getState().isActive())) {
                filtered.add(grant);
            }
        }
        return filtered;
    }

    @Override
    public void deleteByBuses(@NotNull List<String> busesToDelete) throws BackplaneServerException, TokenException {
        throw new NotImplementedException();
    }

    @Override
    public void revokeBuses(Set<Grant> grants, String buses) throws BackplaneServerException, TokenException {
        throw new NotImplementedException();
    }

    @Override
    public Grant get(String id) throws BackplaneServerException {
        byte[] bytes = Redis.getInstance().get(getKey(id));
        if (bytes != null) {
            return (Grant) SerializationUtils.deserialize(bytes);
        } else {
            return null;
        }
    }

    @Override
    public List<Grant> getAll() throws BackplaneServerException {
        List<byte[]> listOfBytes = Redis.getInstance().lrange(getKey("list"), 0, -1);
        List<Grant> grants = new ArrayList<Grant>();
        for (byte[] bytes : listOfBytes) {
            if (bytes != null) {
                grants.add((Grant) SerializationUtils.deserialize(bytes));
            }
        }
        return grants;
    }

    @Override
    public void persist(Grant obj) throws BackplaneServerException {
        byte[] bytes = SerializationUtils.serialize(obj);
        Redis.getInstance().rpush(getKey("list"), bytes);
        Redis.getInstance().set(getKey(obj.getIdValue()), bytes);
    }

    @Override
    public void delete(String id) throws BackplaneServerException, TokenException {
        Jedis jedis = null;
        try {
            jedis = Redis.getInstance().getJedis();
            byte[] bytes = jedis.get(getKey(id));
            if (bytes != null) {
                jedis.lrem(getKey("list"), 0, bytes);
                jedis.del(getKey(id));
            }
        } finally {
            Redis.getInstance().releaseToPool(jedis);
        }
    }
}
