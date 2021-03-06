package com.janrain.backplane.server.dao.redis;

import com.janrain.backplane.server.BackplaneServerException;
import com.janrain.backplane.server.config.BpServerConfig;
import com.janrain.backplane.server.dao.DAO;
import com.janrain.commons.util.SerializationUtils;
import com.janrain.redis.Redis;
import org.apache.commons.lang.NotImplementedException;
import org.apache.log4j.Logger;
import org.jetbrains.annotations.Nullable;

import java.util.List;

/**
 * @author Tom Raney
 */
public class RedisConfigDAO extends DAO<BpServerConfig> {

    public static byte[] getKey(String id) {
        return ("config_" + id).getBytes();
    }

    @Override
    public BpServerConfig get(@Nullable String id)  {
        if (id != null) {
            return SerializationUtils.fromBytes(Redis.getInstance().get(getKey(id)));
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

        logger.info("writing key to redis: " + new String(getKey(obj.getIdValue())));
        byte[] bytes = org.apache.commons.lang.SerializationUtils.serialize(obj);
        Redis.getInstance().set(getKey(obj.getIdValue()), bytes);
    }

    @Override
    public void delete(String id) throws BackplaneServerException {
        throw new NotImplementedException();
    }

    private static final Logger logger = Logger.getLogger(RedisConfigDAO.class);


}
