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

import com.janrain.backplane2.server.BackplaneMessage;
import com.janrain.backplane2.server.Token;
import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.backplane2.server.config.BusConfig2;
import com.janrain.backplane2.server.config.Client;
import com.janrain.backplane2.server.config.User;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import org.springframework.context.annotation.Scope;

import javax.annotation.PostConstruct;
import javax.inject.Inject;

/**
 * @author Tom Raney
 */

@Scope(value="singleton")
public class DaoFactory {

    // - PUBLIC

    public BusDAO getBusDao() {
        return busDao;
    }

    public TokenDAO getTokenDao() {
        return tokenDAO;
    }

    public GrantDAO getGrantDao() {
        return grantDAO;
    }

    public BusOwnerDAO getBusOwnerDAO() {
        return busOwnerDAO;
    }

    public ClientDAO getClientDAO() {
        return clientDAO;
    }

    public BackplaneMessageDAO getBackplaneMessageDAO() {
        return backplaneMessageDAO;
    }

    public AuthSessionDAO getAuthSessionDAO() {
        return authSessionDAO;
    }

    public AuthorizationRequestDAO getAuthorizationRequestDAO() {
        return authorizationRequestDAO;
    }

    public AuthorizationDecisionKeyDAO getAuthorizationDecisionKeyDAO() {
        return authorizationDecisionKeyDAO;
    }

    public DAO getDaoByObjectType(Class<?> obj) {

        if (Client.class.isAssignableFrom(obj)) {
            return getClientDAO();
        } else if (User.class.isAssignableFrom(obj)) {
            return getBusOwnerDAO();
        } else if (BusConfig2.class.isAssignableFrom(obj)) {
            return getBusDao();
        }

        return null;
    }

    public MessageCache<BackplaneMessage> getMessageCache() {
        return messageCache;
    }

    public ConfigLRUCache<Token> getTokenCache() {
        return tokenCache;
    }

    // - PRIVATE

    @Inject
    private SuperSimpleDB superSimpleDB;

    @Inject
    private Backplane2Config bpConfig;

    private final MessageCache<BackplaneMessage> messageCache = new MessageCache<BackplaneMessage>(0L);
    private final ConfigLRUCache<Token> tokenCache = new ConfigLRUCache<Token>(0L);

    private static final Object initLock = new Object();
    private static BusDAO busDao;
    private static TokenDAO tokenDAO;
    private static GrantDAO grantDAO;
    private static BusOwnerDAO busOwnerDAO;
    private static ClientDAO clientDAO;
    private static BackplaneMessageDAO backplaneMessageDAO;
    private static AuthSessionDAO authSessionDAO;
    private static AuthorizationRequestDAO authorizationRequestDAO;
    private static AuthorizationDecisionKeyDAO authorizationDecisionKeyDAO;


    @SuppressWarnings("AssignmentToStaticFieldFromInstanceMethod")
    @PostConstruct
    private void init() {
        messageCache.setMaxCacheSizeBytes(bpConfig.getMaxMessageCacheBytes());
        tokenCache.setMaxCacheSizeBytes(bpConfig.getMaxMessageCacheBytes()); // todo: token cache size config

        synchronized (initLock) {
            busDao = new BusDAO(superSimpleDB, bpConfig, this);
            tokenDAO = new TokenDAO(superSimpleDB, bpConfig, this);
            grantDAO = new GrantDAO(superSimpleDB, bpConfig, this);
            busOwnerDAO = new BusOwnerDAO(superSimpleDB, bpConfig, this);
            clientDAO = new ClientDAO(superSimpleDB, bpConfig, this);
            backplaneMessageDAO = new BackplaneMessageDAO(superSimpleDB, bpConfig, this);
            authSessionDAO = new AuthSessionDAO(superSimpleDB, bpConfig);
            authorizationRequestDAO = new AuthorizationRequestDAO(superSimpleDB, bpConfig);
            authorizationDecisionKeyDAO = new AuthorizationDecisionKeyDAO(superSimpleDB, bpConfig);
        }
    }
}
