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

package com.janrain.backplane.server;

import com.janrain.backplane.server.config.BackplaneConfig;
import com.janrain.backplane.server.config.Client;
import com.janrain.backplane.server.dao.DaoFactory;
import com.janrain.commons.supersimpledb.SimpleDBException;
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
    AuthCode code;
    Client client;
    Scope scopes;
    DaoFactory daoFactory;

    @Inject
    private BackplaneConfig bpConfig;

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
            setCode(daoFactory.getGrantDao().retrieveCode(codeId));
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

    public AuthCode getCode() {
        return this.code;
    }

    public void setCode(AuthCode code) {
        this.code = code;
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
            if (!callback.matches("[a-zA-Z0-9]*")) {
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
            //did a record pull from the db?
            if (code == null) {
                return error("invalid_grant", "Authorization code is invalid");
            }
            if (this.code.isExpired()) {
                return error("invalid_grant", "Authorization code is expired");
            }

            if (!code.isValid()) {
                // revoke related token, if one exists
                daoFactory.getTokenDao().revokeTokenByGrant(code.getGrantId());
                return error("invalid_grant", "Authorization code is invalid");
            }

            //check the client
            if (client == null ||
                    !client_secret.equals(client.getClientSecret()) ||
                    !redirect_uri.equals(client.getRedirectUri()) ||
                    !code.getGrant().getGrantClientId().equals(client.getClientId())) {
                return error("invalid_client", "Client authentication failed");
            }
        }

        if (StringUtils.isNotEmpty(scope)) {
            try {
                this.scopes = new Scope(scope);
                if (this.code != null && !this.code.getGrant().isAllowedBuses(scopes.getBusesInScope())) {
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


