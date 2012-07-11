/*
 * Copyright 2012 Janrain, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.janrain.backplane2.server.dao.simpledb;

import com.janrain.backplane2.server.BackplaneServerException;
import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.backplane2.server.config.BusConfig2;
import com.janrain.backplane2.server.dao.BusDAO;
import com.janrain.backplane2.server.dao.DAOFactory;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import com.janrain.oauth2.TokenException;

import java.util.ArrayList;
import java.util.List;

import static com.janrain.backplane2.server.config.Backplane2Config.SimpleDBTables.BP_BUS_CONFIG;

/**
 * @author Johnny Bufu
 */
public class SimpleDBBusDAO implements BusDAO {

    SimpleDBBusDAO(SuperSimpleDB superSimpleDB, Backplane2Config bpConfig, DAOFactory daoFactory) {
        this.daoFactory = daoFactory;
        this.bpConfig = bpConfig;
        this.superSimpleDB = superSimpleDB;
    }

    @Override
    public BusConfig2 get(String id) throws BackplaneServerException {
        try {
            return superSimpleDB.retrieve(bpConfig.getTableName(BP_BUS_CONFIG), BusConfig2.class, id);
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }

    @Override
    public List<BusConfig2> getAll() throws BackplaneServerException {
        try {
            return superSimpleDB.retrieveAll(bpConfig.getTableName(BP_BUS_CONFIG), BusConfig2.class);
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }

    @Override
    public void persist(BusConfig2 bus) throws BackplaneServerException {
        try {
            superSimpleDB.store(bpConfig.getTableName(BP_BUS_CONFIG), BusConfig2.class, bus);
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }

    @Override
    public void delete(final String id) throws BackplaneServerException, TokenException {
        try {
            superSimpleDB.delete(bpConfig.getTableName(BP_BUS_CONFIG), id);
            daoFactory.getGrantDao().deleteByBuses(new ArrayList<String>() {{add(id);}});
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }

    @Override
    public List<BusConfig2> retrieveByOwner(String busOwner) throws BackplaneServerException {
        try {
            return superSimpleDB.retrieveWhere(
                    bpConfig.getTableName(BP_BUS_CONFIG), BusConfig2.class,
                    BusConfig2.Field.OWNER.getFieldName() + "='" + busOwner +"'", true);
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }

    /** Associated grants and tokens are deleted/revoked. */
    @Override
    public void deleteByOwner(String busOwner) throws BackplaneServerException, TokenException {
        try {
            List<BusConfig2> busConfigs = retrieveByOwner(busOwner);
            List<String> buses = new ArrayList<String>();
            for (BusConfig2 busConfig : busConfigs) {
                buses.add(busConfig.getIdValue());
            }
            superSimpleDB.deleteWhere(bpConfig.getTableName(BP_BUS_CONFIG), BusConfig2.Field.OWNER.getFieldName() + "='" + busOwner +"'");
            daoFactory.getGrantDao().deleteByBuses(buses);
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }

    // - PRIVATE

    private final DAOFactory daoFactory;
    private final SuperSimpleDB superSimpleDB;
    private final Backplane2Config bpConfig;

}
