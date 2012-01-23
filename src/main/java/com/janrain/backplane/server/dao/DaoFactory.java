package com.janrain.backplane.server.dao;

import com.janrain.backplane.server.config.BackplaneConfig;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import org.springframework.context.annotation.Scope;

import javax.inject.Inject;

/**
 * @author Tom Raney
 */

@Scope(value="singleton")
public class DaoFactory {

    public TokenDAO getTokenDao() {
        return new TokenDAO(superSimpleDB, bpConfig);
    }

    public GrantDAO getGrantDao() {
        return new GrantDAO(superSimpleDB, bpConfig);
    }

    public ClientDAO getClientDAO() {
        return new ClientDAO(superSimpleDB, bpConfig);
    }

    public BackplaneMessageDAO getBackplaneMessageDAO() {
        return new BackplaneMessageDAO(superSimpleDB, bpConfig);
    }

    @Inject
    private SuperSimpleDB superSimpleDB;

    @Inject
    private BackplaneConfig bpConfig;

}
