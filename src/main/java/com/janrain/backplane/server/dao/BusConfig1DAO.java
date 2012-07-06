package com.janrain.backplane.server.dao;

import com.janrain.backplane.server.BackplaneServerException;
import com.janrain.backplane.server.BusConfig1;
import com.janrain.redis.Redis;
import org.apache.commons.lang.NotImplementedException;
import org.apache.commons.lang.SerializationUtils;
import org.apache.log4j.Logger;

import java.util.List;

/**
 * @author Tom Raney
 */
public class BusConfig1DAO extends DAO<BusConfig1> {

    public static byte[] getBusKey(String busId) {
        return ("v1_bus_" + busId).getBytes();
    }

    BusConfig1DAO() {
        super();
    }

    private static final Logger logger = Logger.getLogger(BusConfig1DAO.class);

    @Override
    public void persist(BusConfig1 busConfig1) throws BackplaneServerException {
        Redis.getInstance().set(getBusKey(busConfig1.getBusName()), SerializationUtils.serialize(busConfig1));
    }

    @Override
    public void delete(String id) throws BackplaneServerException {
        throw new NotImplementedException();
    }

    @Override
    public BusConfig1 get(String bus) {
        byte[] bytes = Redis.getInstance().get(getBusKey(bus));
        if (bytes != null) {
            return (BusConfig1) SerializationUtils.deserialize(bytes);
        } else {
            return null;
        }
    }

    @Override
    public List<BusConfig1> getAll() throws BackplaneServerException {
        throw new NotImplementedException();
    }

}
