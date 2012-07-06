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

import com.janrain.backplane2.server.*;
import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.backplane2.server.dao.DAOFactory;
import com.janrain.backplane2.server.dao.GrantDAO;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import com.janrain.oauth2.TokenException;
import com.yammer.metrics.Metrics;
import org.apache.commons.lang.NotImplementedException;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.concurrent.Callable;
import java.util.concurrent.TimeUnit;

import static com.janrain.backplane2.server.config.Backplane2Config.SimpleDBTables.BP_GRANT;

/**
 * @author Tom Raney
 */

public class SimpleDBGrantDAO implements GrantDAO {

    SimpleDBGrantDAO(SuperSimpleDB superSimpleDB, Backplane2Config bpConfig, DAOFactory daoFactory) {
        this.daoFactory = daoFactory;
        this.superSimpleDB = superSimpleDB;
        this.bpConfig = bpConfig;
    }

    @Override
    public Grant get(String id) throws BackplaneServerException {
        try {
            return superSimpleDB.retrieve(bpConfig.getTableName(BP_GRANT), Grant.class, id);
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }

    @Override
    public List<Grant> getAll() throws BackplaneServerException {
        throw new NotImplementedException();
    }

    @Override
    public void persist(Grant grant) throws BackplaneServerException {
        try {
            superSimpleDB.store(bpConfig.getTableName(BP_GRANT), Grant.class, grant, true);
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }

    /** Tokens issued against the deleted grant will also be revoked/deleted */
    @Override
    public void delete(String id) throws BackplaneServerException {
        try {
            daoFactory.getTokenDao().revokeTokenByGrant(id);
            superSimpleDB.delete(bpConfig.getTableName(BP_GRANT), id);
            logger.info("Deleted grant (and revoked tokens): " + id);
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }


    @Override
    public void update(Grant grant) throws BackplaneServerException, TokenException {
        //can't make atomic - hope for the best
        delete(grant.getIdValue());
        persist(grant);
    }

    @Override
    public List<Grant> getByClientId(String clientId) throws BackplaneServerException {
        try {
            return superSimpleDB.retrieveWhere(bpConfig.getTableName(BP_GRANT), Grant.class,
                    Grant.GrantField.ISSUED_TO_CLIENT_ID.getFieldName() + "='" + clientId + "' AND " +
                            Grant.GrantField.STATE.getFieldName() + "='" + GrantState.ACTIVE.toString() + "'", true);
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }

    public void deleteByBuses(@NotNull List<String> busesToDelete) throws BackplaneServerException, TokenException {
        try {
            // todo: consider multiple 'select .. like .. ' queries/clauses
            Scope deleteBusesScope = new Scope(Scope.getEncodedScopesAsString(BackplaneMessage.Field.BUS, busesToDelete));
            for(Grant grant : superSimpleDB.retrieveAll(bpConfig.getTableName(BP_GRANT), Grant.class)) {
                Set<String> grantBuses = grant.getAuthorizedScope().getScopeFieldValues(BackplaneMessage.Field.BUS);
                if (grantBuses == null) continue;
                for(String bus : grantBuses) {
                    if (busesToDelete.contains(bus)) {
                        revokeBuses(grant, deleteBusesScope);
                    }
                }
            }
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }


    /**
     *  Revokes buses across the provided grants.
     *  Not atomic, best effort.
     *  Stops on first error and reports error, even though some grants may have been updated.
     */
    public void revokeBuses(Set<Grant> grants, String buses) throws BackplaneServerException, TokenException {
        Scope busesToRevoke = new Scope(Scope.getEncodedScopesAsString(BackplaneMessage.Field.BUS, buses));
        boolean changes = false;
        for (Grant grant : grants) {
            changes = changes || revokeBuses(grant, busesToRevoke);
        }
        if (!changes) {
            throw new BackplaneServerException("No grants found to revoke for buses: " + buses);
        }
    }

    // - PRIVATE

    private static final Logger logger = Logger.getLogger(SimpleDBGrantDAO.class);

    private final DAOFactory daoFactory;
    private final SuperSimpleDB superSimpleDB;
    private final Backplane2Config bpConfig;

    private final com.yammer.metrics.core.Timer v2grantActivateTimer = Metrics.newTimer(SimpleDBGrantDAO.class, "v2_sdb_grant_activate", TimeUnit.MILLISECONDS, TimeUnit.MINUTES);
    private final com.yammer.metrics.core.Timer v2grantClientsTimer = Metrics.newTimer(SimpleDBGrantDAO.class, "v2_sdb_grant_clients", TimeUnit.MILLISECONDS, TimeUnit.MINUTES);

    private boolean revokeBuses(Grant grant, Scope busesToRevoke) throws BackplaneServerException {
        try {
            Scope grantScope = grant.getAuthorizedScope();
            Scope updatedScope = Scope.revoke(grantScope, busesToRevoke);
            if (updatedScope.equals(grantScope)) return false;
            if (!updatedScope.isAuthorizationRequired()) {
                logger.info("Revoked all buses from grant: " + grant.getIdValue());
                delete(grant.getIdValue());
            } else {
                Grant updated = new Grant.Builder(grant, grant.getState()).scope(updatedScope).buildGrant();
                update(grant, updated);
                logger.info("Buses updated updated for grant: " + updated.getIdValue() + " remaining scope: '" + updated.getAuthorizedScope() + "'");
            }
            return true;
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }

    private void update(Grant grant, Grant updated) throws SimpleDBException, BackplaneServerException {
        daoFactory.getTokenDao().revokeTokenByGrant(updated.getIdValue());
        superSimpleDB.update(bpConfig.getTableName(BP_GRANT), Grant.class, grant, updated);
        logger.info("Updated grant (and revoked tokens): " + updated.getIdValue());
    }
}
