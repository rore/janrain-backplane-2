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
import com.janrain.backplane2.server.dao.ConfigLRUCache;
import com.janrain.backplane2.server.dao.TokenDAO;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import com.janrain.oauth2.TokenException;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;

import java.util.ArrayList;
import java.util.Date;
import java.util.LinkedHashSet;
import java.util.List;

import static com.janrain.backplane2.server.config.Backplane2Config.SimpleDBTables.BP_ACCESS_TOKEN;
import static com.janrain.backplane2.server.config.Backplane2Config.SimpleDBTables.BP_REVOKED_TOKEN;

/**
 * @author Tom Raney
 */

public class SimpleDBTokenDAO implements TokenDAO {

    SimpleDBTokenDAO(SuperSimpleDB superSimpleDB, Backplane2Config bpConfig) {
        this.superSimpleDB = superSimpleDB;
        this.bpConfig = bpConfig;
    }

    @Override
    public Token get(String id) throws BackplaneServerException {
        if (! Token.looksLikeOurToken(id)) {
            logger.error("invalid token! => '" + id + "'");
            return null; //invalid token id, don't even try
        }
        try {
            Token cached = tokenCache.get(id);
            if (cached != null) return cached;
            if (tokenCache.isCached(id)) return tokenCache.get(id);
            Token token = superSimpleDB.retrieve(bpConfig.getTableName(BP_ACCESS_TOKEN), Token.class, id);
            if (token == null || ! token.getType().isRefresh()) tokenCache.add(id, token);
            return token;
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage(), e);
        }
    }

    @Override
    public List<Token> getAll() throws BackplaneServerException {
        try {
            return superSimpleDB.retrieveAll(bpConfig.getTableName(BP_ACCESS_TOKEN), Token.class);
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage(), e);
        }
    }

    @Override
    public void persist(final Token token) throws BackplaneServerException {
        try {
            superSimpleDB.store(bpConfig.getTableName(BP_ACCESS_TOKEN), Token.class, token, true);
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage(), e);
        }
    }

    @Override
    public void delete(final String tokenId) throws BackplaneServerException {
        try {
            superSimpleDB.delete(bpConfig.getTableName(BP_ACCESS_TOKEN), tokenId);
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage(), e);
        }
    }

    @Override
    public void deleteExpiredTokens() throws BackplaneServerException {
        try {
            logger.info("Backplane token cleanup task started.");
            String expiredClause = Token.TokenField.EXPIRES.getFieldName() + " < '" + Backplane2Config.ISO8601.get().format(new Date(System.currentTimeMillis())) + "'";
            superSimpleDB.deleteWhere(bpConfig.getTableName(BP_ACCESS_TOKEN), expiredClause);
            superSimpleDB.deleteWhere(bpConfig.getTableName(BP_REVOKED_TOKEN), expiredClause);
        } catch (Exception e) {
            // catch-all, else cleanup thread stops
            logger.error("Backplane token cleanup task error: " + e.getMessage(), e);
        } finally {
            logger.info("Backplane token cleanup task finished.");
        }
    }

    @Override
    public void cacheRevokedCleanup() throws SimpleDBException {
        for(Token revoked : superSimpleDB.retrieveWhere(bpConfig.getTableName(BP_REVOKED_TOKEN), Token.class, null, true)) {
            tokenCache.delete(revoked.getIdValue());
        }
    }

    @Override
    public List<Token> retrieveTokensByGrant(String grantId) throws BackplaneServerException {
        try {
            ArrayList<Token> tokens = new ArrayList<Token>();
            for(Token token : superSimpleDB.retrieveWhere(bpConfig.getTableName(BP_ACCESS_TOKEN), Token.class,
                    Token.TokenField.BACKING_GRANTS.getFieldName() + " LIKE '%" + grantId + "%'", true)) {
                if (token.getBackingGrants().contains(grantId)) {
                    tokens.add(token);
                }
            }
            return tokens;
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage(), e);
        }
    }

    @Override
    public void revokeTokenByGrant(String grantId) throws BackplaneServerException {
        try {
            List<Token> tokens = retrieveTokensByGrant(grantId);
            for (Token token : tokens) {
                delete(token.getIdValue());
                superSimpleDB.store(bpConfig.getTableName(BP_REVOKED_TOKEN), Token.class, token, true);
                logger.info("revoked token " + token.getIdValue());
            }
            if (! tokens.isEmpty()) {
                logger.info("all tokens for grant " + grantId + " have been revoked");
            }
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage(), e);
        }

    }

    /**
     * @return the bus to which this channel is bound, never null
     *
     * @throws com.janrain.commons.supersimpledb.SimpleDBException
     * @throws com.janrain.oauth2.TokenException if the channel is invalid and was bound to more than one bus
     */
    @Override
    public String getBusForChannel(String channel) throws BackplaneServerException, TokenException {
        if (StringUtils.isEmpty(channel)) return null;
        final Scope singleChannelScope = new Scope(BackplaneMessage.Field.CHANNEL, channel);
        try {
            List<Token> tokens = superSimpleDB.retrieveWhere(bpConfig.getTableName(BP_ACCESS_TOKEN), Token.class,
                                Token.TokenField.TYPE.getFieldName() + "='" + GrantType.ANONYMOUS + "' AND " +
                                Token.TokenField.SCOPE.getFieldName() + " LIKE '%" + singleChannelScope.toString() + "%'", true);

            if (tokens == null || tokens.isEmpty()) {
                logger.error("No anonymous tokens found to bind channel " + channel + " to a bus");
                throw new TokenException("invalid channel: " + channel);
            }

            String bus = null;
            for (Token token : tokens) {
                Scope tokenScope = token.getScope();
                if ( tokenScope.containsScope(singleChannelScope) ) {
                    LinkedHashSet<String> buses = tokenScope.getScopeMap().get(BackplaneMessage.Field.BUS);
                    if (buses == null || buses.isEmpty()) continue;
                    if ( (bus != null && buses.size() == 1) || (buses.size() > 1) )  {
                        throw new TokenException("invalid channel, bound to more than one bus");
                    }
                    bus = buses.iterator().next();
                }
            }
            if (bus == null) {
                throw new TokenException("invalid channel: " + channel);
            } else {
                return bus;
            }

        } catch (TokenException te) {
            throw te;
        } catch (Exception e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }

    @Override
    public void bindChannel(String channel, String bus, Integer expireSeconds) {
        // noop
    }

    @Override
    public boolean isValidBinding(String channel, String bus) throws BackplaneServerException {
        try {
            return bus != null && channel != null && bus.equals(getBusForChannel(channel));
        } catch (TokenException e) {
            logger.error(e.getMessage(), e);
            return false;
        }
    }

    // - PRIVATE

    private static final Logger logger = Logger.getLogger(SimpleDBTokenDAO.class);

    private final SuperSimpleDB superSimpleDB;
    private final Backplane2Config bpConfig;

    private final ConfigLRUCache<Token> tokenCache = new ConfigLRUCache<Token>(0L);
}
