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

package com.janrain.oauth2;

import com.janrain.backplane2.server.*;
import com.janrain.backplane2.server.dao.DaoFactory;
import com.janrain.commons.supersimpledb.SimpleDBException;
import org.apache.log4j.Logger;

import java.util.Date;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static com.janrain.oauth2.OAuth2.*;

/**
 * Response
 * @author Tom Raney
 */
public class TokenResponse {

    public TokenResponse(TokenRequest request, DaoFactory daoFactory) {
        this.request = request;
        this.daoFactory = daoFactory;
    }

    public Map<String, Object> generateResponse() throws SimpleDBException, TokenException {
        if (request.grant_type.equals(OAuth2.OAUTH2_TOKEN_GRANT_TYPE_CLIENT_CREDENTIALS) && request.isAnonymousClient()) {
            logger.info("Responding to anonymous token request...");
            // issue new channel id
            final TokenAnonymous token = new TokenAnonymous(request.bus, request.scope, new Date(System.currentTimeMillis() +
                    TokenAnonymous.EXPIRES_SECONDS * 1000L));
            daoFactory.getTokenDao().persist(token);
            return new LinkedHashMap<String, Object>() {{
                put(OAUTH2_ACCESS_TOKEN_PARAM_NAME, token.getIdValue());
                put("expires_in", TokenAnonymous.EXPIRES_SECONDS);
                put(OAUTH2_TOKEN_TYPE_PARAM_NAME, Token.getTokenType());
                put(TokenAnonymous.Field.BUS.getFieldName(), token.getBus());
                //put("backplane_channel", token.getChannelName());
                if (token.mustReturnScopeInResponse()) {
                    put(OAUTH2_SCOPE_PARAM_NAME, token.getScopeString());
                }
            }};
        }

        if (request.grant_type.equals(OAUTH2_TOKEN_GRANT_TYPE_AUTH_CODE)) {
            logger.info("Responding to authorization_code token request...");
            // one grant per authorization_code grant type, enforced not null in TokenRequest validation, no extra checks needed here
            final TokenPrivileged token = new TokenPrivileged(request.client.getClientId(), request.client.getSourceUrl(), request.getGrant(), request.scope);
            daoFactory.getTokenDao().persist(token);
            return new LinkedHashMap<String, Object>() {{
                put(OAUTH2_ACCESS_TOKEN_PARAM_NAME, token.getIdValue());
                put(OAUTH2_TOKEN_TYPE_PARAM_NAME, Token.getTokenType());
                if (token.mustReturnScopeInResponse() && token.getScopeString() == null) {
                    throw new TokenException("A server error has occurred");
                }
                if (token.mustReturnScopeInResponse()) {
                    put(OAUTH2_SCOPE_PARAM_NAME, token.getScopeString());
                }
            }};
        }

        if (request.grant_type.equals(OAuth2.OAUTH2_TOKEN_GRANT_TYPE_CLIENT_CREDENTIALS)) {
            logger.info("Responding to client_credentials token request...");
            Scope scope = new Scope(request.scope);
            List<Grant> grants = daoFactory.getGrantDao().retrieveGrants(request.client.getClientId(), scope);

            // if not all buses in requested scope are granted access, return invalid_scope error (13.1)
            if (! Grant.getBusesAsList(grants).containsAll(scope.getBusesInScope())) {
                throw new TokenException(OAuth2.OAUTH2_TOKEN_INVALID_SCOPE, "requested scope exceeds grants");
            }
            final TokenPrivileged token = new TokenPrivileged(request.client.getClientId(),
                    request.client.getSourceUrl(), grants, request.scope);
            daoFactory.getTokenDao().persist(token);
            return new LinkedHashMap<String, Object>() {{
                put(OAUTH2_ACCESS_TOKEN_PARAM_NAME, token.getIdValue());
                put(OAUTH2_TOKEN_TYPE_PARAM_NAME, Token.getTokenType());
                if (token.mustReturnScopeInResponse() && token.getScopeString() == null) {
                    throw new TokenException("A server error has occurred");
                }
                if (token.mustReturnScopeInResponse()) {
                    put(OAUTH2_SCOPE_PARAM_NAME, token.getScopeString());
                }
            }};
        }

        return null;
    }

    // - PRIVATE

    private static final Logger logger = Logger.getLogger(TokenResponse.class);

    private final TokenRequest request;
    private final DaoFactory daoFactory;
}
