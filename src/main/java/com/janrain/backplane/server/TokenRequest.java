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

import com.janrain.backplane.server.config.AuthException;
import com.janrain.backplane.server.config.BackplaneConfig;
import com.janrain.backplane.server.config.Client;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import com.janrain.crypto.ChannelUtil;
import org.apache.commons.lang.StringUtils;
import org.jetbrains.annotations.NotNull;

import javax.inject.Inject;
import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;

/**
 * Token request
 * @author Tom Raney
 */

public class TokenRequest {

    final String client_id;
    final String grant_type;
    final String redirect_uri;
    final String code;
    final String client_secret;
    final String scope;
    final String callback;
    Code authCode;
    Client client;

    @Inject
    private BackplaneConfig bpConfig;

    private static final String ERR_MSG_FIELD = "error";
    private static final String ERR_MSG_DESCRIPTION = "error_description";

    TokenRequest(String client_id, String grant_type, String redirect_uri, String code, String client_secret, String scope, String callback) {
        this.client_id = client_id;
        this.grant_type = grant_type;
        this.redirect_uri = redirect_uri;
        this.code = code;
        this.client_secret = client_secret;
        this.scope = scope;
        this.callback = callback;
    }

    public Code getCode() {
        return this.authCode;
    }

    public void setCode(Code authCode) {
        this.authCode = authCode;
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

        if (StringUtils.isEmpty(client_id)) {
            return error("invalid_request", "Missing value for client_id");
        }

        if (!grant_type.equals("client_credentials") && !grant_type.equals("code")) {
            return error("invalid_request", "Invalid grant type");
        }

        if ((grant_type.equals("code") && StringUtils.isEmpty(code)) || (grant_type.equals("client_credentials") && StringUtils.isNotEmpty(code)) ) {
            return error("invalid_request", "Missing code value");
        }

        if (grant_type.equals("client_credentials") && !client_id.equals("anonymous") && StringUtils.isEmpty(client_secret)) {
            return error("invalid_request", "Missing client_secret value");
        }

        if (grant_type.equals("code") && StringUtils.isEmpty(redirect_uri)) {
            return error("invalid_request", "Missing redirect_uri value");
        }

        if (grant_type.equals("client_credentials") && client_id.equals("anonymous") && StringUtils.isNotEmpty(client_secret)) {
            return error("invalid_request", "Must not include client_secret for anonymous requests");
        }

        // check the code
        if (StringUtils.isNotEmpty(code)) {
            //did a record pull from the db?
            if (authCode == null) {
                return error("invalid_grant", "Authorization code is invalid");
            }
            if (this.authCode.isExpired()) {
                return error("invalid_grant", "Authorization code is expired");
            }

            //check the client
            if (client == null || !client_secret.equals(client.getClientSecret()) || !redirect_uri.equals(client.getRedirectUri())) {
                return error("invalid_client", "Client authentication failed");
            }
        }

        return null;
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


