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

import com.janrain.oauth2.AuthorizationRequest;
import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import org.apache.log4j.Logger;

import java.util.Date;

import static com.janrain.backplane2.server.config.Backplane2Config.SimpleDBTables.BP_AUTHORIZATION_REQUEST;

/**
 * @author Johnny Bufu
 */
public class AuthorizationRequestDAO extends DAO<AuthorizationRequest> {

    AuthorizationRequestDAO(SuperSimpleDB superSimpleDB, Backplane2Config bpConfig) {
        super(superSimpleDB, bpConfig);
    }

    @Override
    public void persist(AuthorizationRequest authorizationRequest) throws SimpleDBException {
        superSimpleDB.store(bpConfig.getTableName(BP_AUTHORIZATION_REQUEST), AuthorizationRequest.class, authorizationRequest);
    }

    @Override
    public void delete(String id) throws SimpleDBException {
        superSimpleDB.delete(bpConfig.getTableName(BP_AUTHORIZATION_REQUEST), id);
    }

    public AuthorizationRequest retrieveAuthorizationRequest(String cookie) throws SimpleDBException {
        return superSimpleDB.retrieve(bpConfig.getTableName(BP_AUTHORIZATION_REQUEST), AuthorizationRequest.class, cookie);
    }

    public void deleteExpiredAuthorizationRequests() {
        try {
            logger.info("Backplane authorization requests cleanup task started.");
            String expiredClause = AuthorizationRequest.Field.EXPIRES.getFieldName() + " < '" + Backplane2Config.ISO8601.format(new Date(System.currentTimeMillis())) + "'";
            superSimpleDB.deleteWhere(bpConfig.getTableName(BP_AUTHORIZATION_REQUEST), expiredClause);
        } catch (Exception e) {
            // catch-all, else cleanup thread stops
            logger.error("Backplane authorization requests cleanup task error: " + e.getMessage(), e);
        } finally {
            logger.info("Backplane authorization requests cleanup task finished.");
        }
    }

    // - PRIVATE

    private static final Logger logger = Logger.getLogger(AuthorizationRequestDAO.class);

}
