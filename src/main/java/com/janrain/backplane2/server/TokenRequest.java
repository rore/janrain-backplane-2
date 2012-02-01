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

package com.janrain.backplane2.server;

import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.backplane2.server.config.Client;
import com.janrain.backplane2.server.dao.DaoFactory;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.crypto.HmacHashUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import javax.inject.Inject;
import java.util.HashMap;

/**
 * Token request
 * @author Tom Raney
 */

public class TokenRequest {

    String client_id;
    String grant_type;
    String redirect_uri;
    String codeId;
    String client_secret;
    String scope;
    String callback;
    Grant grant;
    Client client;
    Scope scopes;
    DaoFactory daoFactory;

    @Inject
    private Backplane2Config bpConfig;

    private static final String ERR_MSG_FIELD = "error";
    private static final String ERR_MSG_DESCRIPTION = "error_description";

    private static final Logger logger = Logger.getLogger(TokenRequest.class);

    TokenRequest(DaoFactory daoFactory, String client_id, String grant_type, String redirect_uri, String codeId, String client_secret, String scope, String callback) {
        this.client_id = client_id;
        this.grant_type = grant_type;
        this.redirect_uri = redirect_uri;
        this.codeId = codeId;
        this.client_secret = client_secret;
        this.scope = scope;
        this.daoFactory = daoFactory;
        this.callback = callback;

        try {
            setGrant(daoFactory.getGrantDao().retrieveGrant(codeId));
        } catch (Exception e) {
            //do nothing
        }

        try {
            if (StringUtils.isNotEmpty(client_id) && !client_id.equals(Token.ANONYMOUS)) {
                setClient(daoFactory.getClientDAO().retrieveClient(client_id));
            }
        } catch (Exception e) {
            //do nothing
            logger.info("could not retrieve client with id '" + client_id + "'", e);
        }
    }

    TokenRequest(String client_id, String grant_type, String scope, String callback) {
        this.client_id = client_id;
        this.grant_type = grant_type;
        this.scope = scope;
        this.callback = callback;
    }

    public Grant getGrant() {
        return this.grant;
    }

    public void setGrant(Grant grant) {
        this.grant = grant;
    }

    public void setClient(Client client) {
        this.client = client;
    }

    /**
     * Validates input
     * @return null if no errors exist or a HashMap with an error
     */

    public HashMap<String, Object> validate() throws SimpleDBException {
         // use Oauth2 5.2 response codes for errors - http://tools.ietf.org/html/draft-ietf-oauth-v2-22#section-5.2

        if (StringUtils.isNotEmpty(callback)) {
            if (!callback.matches("[\\._a-zA-Z0-9]*")) {
                return error("invalid_request", "callback parameter value is malformed");
            }
        }

        if (StringUtils.isEmpty(client_id)) {
            return error("invalid_request", "Missing value for client_id");
        }

        if (!grant_type.equals("client_credentials") && !grant_type.equals("code")) {
            return error("invalid_request", "Invalid grant type");
        }

        if ((grant_type.equals("code") && StringUtils.isEmpty(codeId)) || (grant_type.equals("client_credentials") && StringUtils.isNotEmpty(codeId)) ) {
            return error("invalid_request", "Missing code value");
        }

        if (grant_type.equals("client_credentials") && !client_id.equals(Token.ANONYMOUS) && StringUtils.isEmpty(client_secret)) {
            return error("invalid_request", "Missing client_secret value");
        }

        if (grant_type.equals("code") && StringUtils.isEmpty(redirect_uri)) {
            return error("invalid_request", "Missing redirect_uri value");
        }

        if (grant_type.equals("client_credentials") && client_id.equals(Token.ANONYMOUS) && StringUtils.isNotEmpty(client_secret)) {
            return error("invalid_request", "Must not include client_secret for anonymous requests");
        }

        // check the codeId
        if (StringUtils.isNotEmpty(codeId)) {
            //did a grant record pull from the db?
            if (grant == null) {
                return error("invalid_grant", "Authorization code is invalid");
            }

            if (grant.isCodeUsed()) {
                // revoke related token, if one exists and has already been used
                daoFactory.getTokenDao().revokeTokenByGrant(grant.getIdValue());
                return error("invalid_grant", "Authorization code is invalid");
            }

            if (this.grant.isCodeExpired()) {
                return error("invalid_grant", "Authorization code is expired");
            }

            //check the client
            if (client == null ||
                    !HmacHashUtils.checkHmacHash(client_secret, client.getClientSecret()) ||
                    !redirect_uri.equals(client.getRedirectUri()) ||
                    !grant.getGrantClientId().equals(client.getClientId())) {
                return error("invalid_client", "Client authentication failed");
            }
        }

        if (StringUtils.isNotEmpty(scope)) {
            try {
                this.scopes = new Scope(scope);
                if (this.grant != null && !grant.isAllowedBuses(scopes.getBusesInScope())) {
                    return error("invalid_request", "Invalid scope");
                }
            } catch (BackplaneServerException e) {
                return error("invalid_request", e.getMessage());
            }
        }

        return new HashMap<String, Object>();
    }

    public HashMap<String, Object> error(@NotNull final String error, final String description) {
        return new HashMap<String, Object>() {{
            put(ERR_MSG_FIELD, error);
            if (description != null) {
                put(ERR_MSG_DESCRIPTION, description);
            }
        }};
    }
}


