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

import com.janrain.backplane2.server.AuthSession;
import com.janrain.backplane2.server.BackplaneServerException;
import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.backplane2.server.dao.AuthSessionDAO;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import org.apache.commons.lang.NotImplementedException;
import org.apache.log4j.Logger;

import java.util.Date;
import java.util.List;

import static com.janrain.backplane2.server.config.Backplane2Config.SimpleDBTables.BP_AUTH_SESSION;

/**
 * @author Johnny Bufu
 */
public class SimpleDBAuthSessionDAO implements AuthSessionDAO {

    SimpleDBAuthSessionDAO(SuperSimpleDB superSimpleDB, Backplane2Config bpConfig) {
        this.superSimpleDB = superSimpleDB;
        this.bpConfig = bpConfig;
    }

    @Override
    public AuthSession get(String id) throws BackplaneServerException {
        try {
            return superSimpleDB.retrieve(bpConfig.getTableName(BP_AUTH_SESSION), AuthSession.class, id);
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }

    @Override
    public List<AuthSession> getAll() throws BackplaneServerException {
        throw new NotImplementedException();
    }

    @Override
    public void persist(AuthSession authSession) throws BackplaneServerException {
        try {
            superSimpleDB.store(bpConfig.getTableName(BP_AUTH_SESSION), AuthSession.class, authSession);
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }

    @Override
    public void delete(String id) throws BackplaneServerException {
        try {
            superSimpleDB.delete(bpConfig.getTableName(BP_AUTH_SESSION), id);
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }

    public void deleteExpiredAuthSessions() {
        try {
            logger.info("Backplane auth sessions cleanup task started.");
            String expiredClause = AuthSession.Field.EXPIRES.getFieldName() + " < '" + Backplane2Config.ISO8601.get().format(new Date(System.currentTimeMillis())) + "'";
            superSimpleDB.deleteWhere(bpConfig.getTableName(BP_AUTH_SESSION), expiredClause);
        } catch (Exception e) {
            // catch-all, else cleanup thread stops
            logger.error("Backplane auth sessions cleanup task error: " + e.getMessage(), e);
        } finally {
            logger.info("Backplane auth sessions cleanup task finished.");
        }
    }

    // - PRIVATE

    private static final Logger logger = Logger.getLogger(SimpleDBAuthSessionDAO.class);

    private SuperSimpleDB superSimpleDB;
    private Backplane2Config bpConfig;

}
