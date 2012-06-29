package com.janrain.backplane.server.dao;

import com.janrain.backplane.server.BusConfig1;
import com.janrain.backplane.server.config.Backplane1Config;
import com.janrain.backplane.server.redis.Redis;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import org.apache.commons.lang.NotImplementedException;
import org.apache.commons.lang.SerializationUtils;
import org.apache.log4j.Logger;

/**
 * @author Tom Raney
 */
public class BusConfig1DAO extends DAO<BusConfig1> {

    public static byte[] getBusKey(String busId) {
        return new String("v1_bus_" + busId).getBytes();
    }

    BusConfig1DAO() {
        super();
    }

    private static final Logger logger = Logger.getLogger(BusConfig1DAO.class);

    @Override
    public void persist(BusConfig1 busConfig1) throws SimpleDBException {
        Redis.getInstance().set(getBusKey(busConfig1.getBusName()), SerializationUtils.serialize(busConfig1));
    }

    @Override
    public void delete(String id) throws SimpleDBException {
        throw new NotImplementedException();
    }

    public BusConfig1 get(String bus) {
        byte[] bytes = Redis.getInstance().get(getBusKey(bus));
        if (bytes != null) {
            return (BusConfig1) SerializationUtils.deserialize(bytes);
        } else {
            return null;
        }
    }

}
