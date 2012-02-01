/*
 * Copyright 2011 Janrain, Inc.
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

import com.janrain.backplane2.server.AuthSession;
import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import org.apache.log4j.Logger;

import java.util.Date;

import static com.janrain.backplane2.server.config.Backplane2Config.SimpleDBTables.BP_ACCESS_TOKEN;
import static com.janrain.backplane2.server.config.Backplane2Config.SimpleDBTables.BP_AUTH_SESSION;

/**
 * @author Johnny Bufu
 */
public class AuthSessionDAO extends DAO {

    AuthSessionDAO(SuperSimpleDB superSimpleDB, Backplane2Config bpConfig) {
        super(superSimpleDB, bpConfig);
    }

    public void persistAuthSession(AuthSession authSession) throws SimpleDBException {
        superSimpleDB.store(bpConfig.getTableName(BP_AUTH_SESSION), AuthSession.class, authSession);
    }

    public AuthSession retrieveAuthSession(String cookie) throws SimpleDBException {
        return superSimpleDB.retrieve(bpConfig.getTableName(BP_AUTH_SESSION), AuthSession.class, cookie);
    }

    public void deleteExpiredAuthSessions() {
        try {
            logger.info("Backplane auth sessions cleanup task started.");
            String expiredClause = AuthSession.Field.EXPIRES.getFieldName() + " < '" + Backplane2Config.ISO8601.format(new Date(System.currentTimeMillis())) + "'";
            superSimpleDB.deleteWhere(bpConfig.getTableName(BP_ACCESS_TOKEN), expiredClause);
        } catch (Exception e) {
            // catch-all, else cleanup thread stops
            logger.error("Backplane auth sessions cleanup task error: " + e.getMessage(), e);
        } finally {
            logger.info("Backplane auth sessions cleanup task finished.");
        }
    }

    // - PRIVATE

    private static final Logger logger = Logger.getLogger(AuthSessionDAO.class);

}
