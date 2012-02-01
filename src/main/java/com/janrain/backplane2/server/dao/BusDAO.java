package com.janrain.backplane2.server.dao;

import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.backplane2.server.config.BusConfig2;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;

import java.util.List;

import static com.janrain.backplane2.server.config.Backplane2Config.SimpleDBTables.BP_BUS_CONFIG;

/**
 * @author Johnny Bufu
 */
public class BusDAO extends DAO {

    BusDAO(SuperSimpleDB superSimpleDB, Backplane2Config bpConfig) {
        super(superSimpleDB, bpConfig);
    }

    public void persistBus(BusConfig2 bus) throws SimpleDBException {
        superSimpleDB.store(bpConfig.getTableName(BP_BUS_CONFIG), BusConfig2.class, bus);
    }

    public List<BusConfig2> retrieveBuses() throws SimpleDBException {
        return superSimpleDB.retrieve(bpConfig.getTableName(BP_BUS_CONFIG), BusConfig2.class);
    }

    public BusConfig2 retrieveBus(String busId) throws SimpleDBException {
        return superSimpleDB.retrieve(bpConfig.getTableName(BP_BUS_CONFIG), BusConfig2.class, busId);
    }

    public List<BusConfig2> retrieveBuses(String busOwner) throws SimpleDBException {
        return superSimpleDB.retrieveWhere(
                bpConfig.getTableName(BP_BUS_CONFIG), BusConfig2.class,
                BusConfig2.Field.OWNER.getFieldName() + "=" + busOwner, true);
    }

    public void deleteBus(String busId) throws SimpleDBException {
        superSimpleDB.delete(bpConfig.getTableName(BP_BUS_CONFIG), busId);
    }
}
