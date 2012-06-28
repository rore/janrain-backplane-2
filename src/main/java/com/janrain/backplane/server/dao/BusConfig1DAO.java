package com.janrain.backplane.server.dao;

import com.janrain.backplane.server.config.Backplane1Config;
import com.janrain.backplane.server.config.BusConfig1;
import com.janrain.backplane.server.redis.Redis;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import org.apache.log4j.Logger;

/**
 * @author Tom Raney
 */
public class BusConfig1DAO extends DAO<BusConfig1> {

    public static byte[] getBusKey(String busId) {
        return ("v1_bus_" + busId).getBytes();
    }

    BusConfig1DAO(SuperSimpleDB superSimpleDB, Backplane1Config bpConfig) {
        super(superSimpleDB, bpConfig);
    }

    private static final Logger logger = Logger.getLogger(BusConfig1DAO.class);

    @Override
    public void persist(BusConfig1 busConfig) throws SimpleDBException {
        Redis.getInstance().set(getBusKey(busConfig.get(BusConfig1.Field.BUS_NAME)), busConfig.toBytes());
    }

    @Override
    public void delete(String id) throws SimpleDBException {
        //To change body of implemented methods use File | Settings | File Templates.
    }

    public BusConfig1 get(String bus) {
        return BusConfig1.fromBytes(Redis.getInstance().get(getBusKey(bus)));
    }

}
