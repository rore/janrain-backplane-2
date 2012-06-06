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
import com.janrain.backplane2.server.Scope;
import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.backplane2.server.config.Client;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import com.janrain.oauth2.TokenException;
import com.yammer.metrics.Metrics;
import org.apache.log4j.Logger;

import java.util.Map;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.TimeUnit;

import static com.janrain.backplane2.server.config.Backplane2Config.SimpleDBTables.BP_CLIENTS;

/**
 * @author Tom Raney
 */
public class ClientDAO extends DAO<Client> {

    ClientDAO(SuperSimpleDB superSimpleDB, Backplane2Config bpConfig, DaoFactory daoFactory) {
        super(superSimpleDB, bpConfig);
        this.daoFactory = daoFactory;
    }

    @Override
    public void persist(Client client) throws SimpleDBException {
        superSimpleDB.store(bpConfig.getTableName(BP_CLIENTS), Client.class, client);
    }

    @Override
    public void delete(String id) throws SimpleDBException {
        try {
            logger.info("=== BEGIN CLIENT DELETE ===");
            superSimpleDB.delete(bpConfig.getTableName(BP_CLIENTS), id);
            Map<Scope,Set<Grant>> clientGrants = daoFactory.getGrantDao().retrieveClientGrants(id, null);
            for(Set<Grant> grants : clientGrants.values()) {
                for (Grant grant : grants) {
                    daoFactory.getGrantDao().delete(grant.getIdValue());
                }
            }
            logger.info("Client " + id + " deleted successfully");
            logger.info("=== END CLIENT DELETE ===");
        } catch (SimpleDBException e) {
            logger.error("An exception occurred during an atomic operation.  Corruption may have occurred while removing client: " + id, e);
            throw e;
        } catch (TokenException e) {
            // should not happen
            logger.error("An exception occurred during an atomic operation.  Corruption may have occurred while removing client: " + id, e);
            throw new SimpleDBException(e);
        }
    }

    public Client retrieveClient(final String client) throws SimpleDBException {
        try {
            return v2clientLookup.time(new Callable<Client>() {
                @Override
                public Client call() throws Exception {
                    return superSimpleDB.retrieve(bpConfig.getTableName(BP_CLIENTS), Client.class, client);
                }
            });
        } catch (SimpleDBException sdbe) {
            throw sdbe;
        } catch (Exception e) {
            throw new SimpleDBException(e);
        }
    }

    // - PRIVATE

    private static final Logger logger = Logger.getLogger(ClientDAO.class);

    private final DaoFactory daoFactory;

    private final com.yammer.metrics.core.Timer v2clientLookup = Metrics.newTimer(ClientDAO.class, "v2_sdb_client_lookup", TimeUnit.MILLISECONDS, TimeUnit.MINUTES);

}
