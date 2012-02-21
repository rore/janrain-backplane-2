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

package com.janrain.backplane2.server.dao;

import com.janrain.backplane2.server.Grant;
import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.backplane2.server.config.Client;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import org.apache.log4j.Logger;

import java.util.List;

import static com.janrain.backplane2.server.config.Backplane2Config.SimpleDBTables.BP_CLIENTS;

/**
 * @author Tom Raney
 */
public class ClientDAO extends DAO {

    ClientDAO(SuperSimpleDB superSimpleDB, Backplane2Config bpConfig) {
        super(superSimpleDB, bpConfig);
    }

    @Override
    public void persist(Object client) throws SimpleDBException {
        superSimpleDB.store(bpConfig.getTableName(BP_CLIENTS), Client.class, (Client) client);
    }

    @Override
    public void delete(String id) throws SimpleDBException {
        try {
            logger.info("=== BEGIN CLIENT DELETE ===");
            superSimpleDB.delete(bpConfig.getTableName(BP_CLIENTS), id);
            GrantDAO grantDao = new GrantDAO(superSimpleDB, bpConfig);
            TokenDAO tokenDao = new TokenDAO(superSimpleDB, bpConfig);
            List<Grant> grants = grantDao.retrieveGrants(id, null);
            for (Grant grant : grants) {
                grantDao.delete(grant.getIdValue());
                tokenDao.revokeTokenByGrant(grant);
            }
            logger.info("Client " + id + " deleted successfully");
            logger.info("=== END CLIENT DELETE ===");
        } catch (SimpleDBException sdbe) {
            logger.error("An exception occurred during an atomic operation.  Corruption may have occurred while removing client: " + id);
            throw sdbe;
        }
    }

    public Client retrieveClient(String client) throws SimpleDBException {
        return superSimpleDB.retrieve(bpConfig.getTableName(BP_CLIENTS), Client.class, client);
    }

    // - PRIVATE

    private static final Logger logger = Logger.getLogger(ClientDAO.class);

}


