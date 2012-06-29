package com.janrain.backplane.server.dao;

import com.janrain.backplane.server.config.Backplane1Config;
import com.janrain.backplane.server.config.BpServerConfig;
import com.janrain.backplane.server.redis.Redis;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;

/**
 * @author Tom Raney
 */
public class ConfigDAO extends DAO<BpServerConfig> {

    ConfigDAO() {
        super();
    }

    public BpServerConfig get() {
        byte[] bytes = Redis.getInstance().get(BpServerConfig.BPSERVER_CONFIG_KEY.getBytes());
        if (bytes != null) {
            return BpServerConfig.fromBytes(bytes);
        } else {
            return null;
        }
    }

    @Override
    public void persist(BpServerConfig obj) throws SimpleDBException {

    }

    @Override
    public void delete(String id) throws SimpleDBException {
        //To change body of implemented methods use File | Settings | File Templates.
    }
}
