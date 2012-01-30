package com.janrain.backplane2.server.dao;

import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import org.springframework.context.annotation.Scope;

import javax.inject.Inject;

/**
 * @author Tom Raney
 */

@Scope(value="singleton")
public class DaoFactory {

    public BusDAO getBusDao() {
        return new BusDAO(superSimpleDB, bpConfig);
    }

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

    public AuthSessionDAO getAuthSessionDAO() {
        return new AuthSessionDAO(superSimpleDB, bpConfig);
    }

    public AuthorizationRequestDAO getAuthorizationRequestDAO() {
        return new AuthorizationRequestDAO(superSimpleDB, bpConfig);
    }

    public AuthorizationDecisionKeyDAO getAuthorizationDecisionKeyDAO() {
        return new AuthorizationDecisionKeyDAO(superSimpleDB, bpConfig);
    }

    @Inject
    private SuperSimpleDB superSimpleDB;

    @Inject
    private Backplane2Config bpConfig;

}
