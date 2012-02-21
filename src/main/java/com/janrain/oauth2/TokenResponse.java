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
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;

/**
 * Response
 * @author Tom Raney
 */
public class TokenResponse {

    TokenRequest request;
    private DaoFactory daoFactory;

    public TokenResponse(TokenRequest request, DaoFactory daoFactory) {
        this.request = request;
        this.daoFactory = daoFactory;
    }

    public HashMap<String, Object> generateResponse() throws SimpleDBException, BackplaneServerException {
        if (request.grant_type.equals("client_credentials") && request.client_id.equals(Token.ANONYMOUS)) {
            // issue new channel id
            final TokenAnonymous token = new TokenAnonymous(null, request.scope, new Date(new Date().getTime() + Token.EXPIRES_SECONDS * 1000L));
            daoFactory.getTokenDao().persist(token);
            return new LinkedHashMap<String, Object>() {{
                put("access_token", token.getIdValue());
                put("expires_in", Token.EXPIRES_SECONDS);
                put("token_type", token.getTokenType());
                put("backplane_channel", token.getChannelName());
            }};
        }

        if (request.grant_type.equals("code")) {
            final TokenPrivileged token = new TokenPrivileged(request.client_id, request.getGrant(), request.scope);
            daoFactory.getTokenDao().persist(token);
            return new LinkedHashMap<String, Object>() {{
                put("access_token", token.getIdValue());
                put("token_type", "Bearer");
                if (token.mustReturnScopeInResponse()) {
                    put("scope", token.getScopeString());
                }
            }};
        }

        if (request.grant_type.equals("client_credentials")) {
            List<Grant> grants = daoFactory.getGrantDao().retrieveGrants(request.client_id, new Scope(request.scope));
            final TokenPrivileged token = new TokenPrivileged(request.client_id, grants, null);
            daoFactory.getTokenDao().persist(token);
            return new LinkedHashMap<String, Object>() {{
                put("access_token", token.getIdValue());
                put("token_type", "Bearer");
                if (token.mustReturnScopeInResponse()) {
                    put("scope", token.getScopeString());
                }
            }};
        }

        return null;
    }


    private static final Logger logger = Logger.getLogger(TokenResponse.class);
}
