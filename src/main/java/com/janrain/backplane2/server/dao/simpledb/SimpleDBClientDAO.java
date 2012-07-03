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
import com.janrain.backplane2.server.Grant;
import com.janrain.backplane2.server.GrantLogic;
import com.janrain.backplane2.server.Scope;
import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.backplane2.server.config.Client;
import com.janrain.backplane2.server.dao.ClientDAO;
import com.janrain.backplane2.server.dao.DAOFactory;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import com.janrain.oauth2.TokenException;
import org.apache.commons.lang.NotImplementedException;
import org.apache.log4j.Logger;

import java.util.List;
import java.util.Map;
import java.util.Set;

import static com.janrain.backplane2.server.config.Backplane2Config.SimpleDBTables.BP_CLIENTS;

/**
 * @author Tom Raney
 */
public class SimpleDBClientDAO implements ClientDAO {

    SimpleDBClientDAO(SuperSimpleDB superSimpleDB, Backplane2Config bpConfig, DAOFactory daoFactory) {
        this.daoFactory = daoFactory;
        this.bpConfig = bpConfig;
        this.superSimpleDB = superSimpleDB;
    }

    @Override
    public Client get(String id) throws BackplaneServerException {
        try {
            return superSimpleDB.retrieve(bpConfig.getTableName(BP_CLIENTS), Client.class, id);
        } catch (Exception e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }

    @Override
    public List<Client> getAll() throws BackplaneServerException {
        throw new NotImplementedException();
    }

    @Override
    public void persist(Client client) throws BackplaneServerException {
        try {
            superSimpleDB.store(bpConfig.getTableName(BP_CLIENTS), Client.class, client);
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }

    @Override
    public void delete(String id) throws BackplaneServerException {
        try {
            logger.info("=== BEGIN CLIENT DELETE ===");
            superSimpleDB.delete(bpConfig.getTableName(BP_CLIENTS), id);
            Map<Scope,Set<Grant>> clientGrants = new GrantLogic(daoFactory).retrieveClientGrants(id, null);
            for(Set<Grant> grants : clientGrants.values()) {
                for (Grant grant : grants) {
                    daoFactory.getGrantDao().delete(grant.getIdValue());
                }
            }
            logger.info("Client " + id + " deleted successfully");
            logger.info("=== END CLIENT DELETE ===");
        } catch (SimpleDBException e) {
            logger.error("An exception occurred during an atomic operation.  Corruption may have occurred while removing client: " + id, e);
            throw new BackplaneServerException(e.getMessage());
        } catch (TokenException e) {
            // should not happen
            logger.error("An exception occurred during an atomic operation.  Corruption may have occurred while removing client: " + id, e);
            throw new BackplaneServerException(e.getMessage());
        }
    }


    // - PRIVATE

    private final DAOFactory daoFactory;
    private final SuperSimpleDB superSimpleDB;
    private final Backplane2Config bpConfig;

    private static final Logger logger = Logger.getLogger(SimpleDBClientDAO.class);
}
