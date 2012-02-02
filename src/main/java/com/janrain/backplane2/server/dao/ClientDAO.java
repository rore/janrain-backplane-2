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

import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.backplane2.server.config.Client;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;

import static com.janrain.backplane2.server.config.Backplane2Config.SimpleDBTables.BP_CLIENTS;

/**
 * @author Tom Raney
 */
public class ClientDAO extends DAO {

    ClientDAO(SuperSimpleDB superSimpleDB, Backplane2Config bpConfig) {
        super(superSimpleDB, bpConfig);
    }


    public void persistClient(Client client) throws SimpleDBException {
        superSimpleDB.store(bpConfig.getTableName(BP_CLIENTS), Client.class, client);
    }

    public Client retrieveClient(String client) throws SimpleDBException {
        return superSimpleDB.retrieve(bpConfig.getTableName(BP_CLIENTS), Client.class, client);
    }
}


