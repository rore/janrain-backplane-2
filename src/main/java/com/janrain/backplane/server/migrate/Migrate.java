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

package com.janrain.backplane.server.migrate;

import com.janrain.backplane.server.config.Backplane1Config;
import com.janrain.backplane.server.dao.BusConfig1DAO;
import com.janrain.backplane.server.config.BusConfig1;
import com.janrain.backplane.server.config.User;
import com.janrain.backplane.server.dao.DaoFactory;
import com.janrain.backplane.server.dao.UserDAO;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import org.apache.log4j.Logger;

import java.util.List;

/**
 * @author Tom Raney
 */
public class Migrate {

    private SuperSimpleDB superSimpleDB;
    private Backplane1Config bpConfig;
    private final DaoFactory daoFactory;

    public Migrate(SuperSimpleDB superSimpleDB, Backplane1Config bpConfig, DaoFactory daoFactory) {
        this.superSimpleDB = superSimpleDB;
        this.bpConfig = bpConfig;
        this.daoFactory = daoFactory;
    }

    public void migrate() throws SimpleDBException {

        List<User> users = superSimpleDB.retrieveAll(bpConfig.getTableName(Backplane1Config.SimpleDBTables.BP1_USERS), User.class);
        int recs = 0;

        UserDAO userDAO = daoFactory.getUserDAO();

        for (User user: users) {
            logger.info("(" + ++recs + ") User records imported: " + user.getIdValue());
            userDAO.persist(user);
        }

        BusConfig1DAO busConfig1DAO = daoFactory.getNewBusDAO();
        List<BusConfig1> buses = superSimpleDB.retrieveAll(bpConfig.getTableName(Backplane1Config.SimpleDBTables.BP1_BUS_CONFIG), BusConfig1.class);
        recs = 0;
        for (BusConfig1 bus: buses) {
            logger.info("(" + ++recs + ") BusConfig records imported: " + bus.getIdValue());
            busConfig1DAO.persist(bus);
        }

        //TODO existing messages?

        logger.info("migration of SDB data complete");


    }

    private static final Logger logger = Logger.getLogger(Migrate.class);
}
