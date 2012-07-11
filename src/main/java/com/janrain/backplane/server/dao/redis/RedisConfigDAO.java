package com.janrain.backplane.server.dao.redis;

import com.janrain.backplane.server.BackplaneServerException;
import com.janrain.backplane.server.config.BpServerConfig;
import com.janrain.backplane.server.dao.DAO;
import com.janrain.commons.util.SerializationUtils;
import com.janrain.redis.Redis;
import org.apache.commons.lang.NotImplementedException;
import org.jetbrains.annotations.Nullable;

import java.util.List;

/**
 * @author Tom Raney
 */
public class RedisConfigDAO extends DAO<BpServerConfig> {

    @Override
    public BpServerConfig get(@Nullable String id)  {
        if (id != null) {
            return SerializationUtils.fromBytes(Redis.getInstance().get(id.getBytes()));
        } else {
            return null;
        }
    }

    @Override
    public List<BpServerConfig> getAll() throws BackplaneServerException {
        throw new NotImplementedException();
    }

    @Override
    public void persist(BpServerConfig obj) throws BackplaneServerException {

    }

    @Override
    public void delete(String id) throws BackplaneServerException {
    }


}
