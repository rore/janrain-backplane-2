package com.janrain.backplane.server.dao;

import com.janrain.backplane.server.config.BackplaneConfig;
import com.janrain.backplane.server.config.Client;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;

/**
 * @author Tom Raney
 */
public class ClientDAO extends DAO {

    ClientDAO(SuperSimpleDB superSimpleDB, BackplaneConfig bpConfig) {
        super(superSimpleDB, bpConfig);
    };


    public void persistClient(Client client) throws SimpleDBException {
        superSimpleDB.store(bpConfig.getClientsTableName(), Client.class, client);
    }

    public Client retrieveClient(String client) throws SimpleDBException {
        return superSimpleDB.retrieve(bpConfig.getClientsTableName(), Client.class, client);
    }


}


