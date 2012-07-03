package com.janrain.backplane.server.dao;

import com.janrain.backplane.server.BackplaneServerException;
import com.janrain.backplane.server.config.BpServerConfig;
import com.janrain.commons.supersimpledb.message.NamedMap;
import com.janrain.redis.Redis;
import com.janrain.commons.supersimpledb.SimpleDBException;
import org.apache.commons.lang.NotImplementedException;

import java.util.List;

/**
 * @author Tom Raney
 */
public class ConfigDAO extends DAO<BpServerConfig> {

    public ConfigDAO() {
        super();
    }

    @Override
    public BpServerConfig get(String id)  {
        byte[] bytes = Redis.getInstance().get(BpServerConfig.BPSERVER_CONFIG_KEY.getBytes());
        if (bytes != null) {
            return BpServerConfig.fromBytes(bytes);
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
