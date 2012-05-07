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

import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.message.MessageField;
import com.janrain.crypto.ChannelUtil;
import com.janrain.oauth2.OAuth2;
import com.janrain.oauth2.TokenException;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;

import java.util.ArrayList;
import java.util.Collections;
import java.util.Date;
import java.util.List;

/**
 * @author Tom Raney
 */
public class TokenPrivileged extends Token {

    public static final long EXPIRES_SECONDS = 31536000;  // 1 year
    public static final long TEST_EXPIRES_SECONDS = 600; // 10 minutes

    /**
     * Empty default constructor for AWS to use.
     */
    public TokenPrivileged() {}

    public TokenPrivileged(String tokenString, String clientId, String clientSourceUrl,
                           String buses, String scopeString, Date expires) throws TokenException, SimpleDBException {

        super(Token.PR + tokenString, TYPE.PRIVILEGED_TOKEN, buses, scopeString, expires);

        put(Field.ISSUED_TO_CLIENT_ID.getFieldName(), clientId);
        put(Field.CLIENT_SOURCE_URL.getFieldName(), clientSourceUrl);

        setScopeString(processScope(scopeString));

        validate();
    }

    public TokenPrivileged(String clientId, String clientSourceUrl, String buses, String scopeString, Date expires) throws TokenException, SimpleDBException {
        this(ChannelUtil.randomString(TOKEN_LENGTH), clientId, clientSourceUrl, buses, scopeString, expires);
    }

    public TokenPrivileged(String clientId, String clientSourceUrl, Grant grant, String scope) throws TokenException, SimpleDBException {
        this(ChannelUtil.randomString(TOKEN_LENGTH), clientId, clientSourceUrl, grant.getBusesAsString(), scope,
                new Date(System.currentTimeMillis() + TokenPrivileged.EXPIRES_SECONDS * 1000));
        this.addGrant(grant);
    }

    public TokenPrivileged(String clientId, String clientSourceUrl, List<Grant> grants, String scope) throws TokenException, SimpleDBException {
        this(ChannelUtil.randomString(TOKEN_LENGTH), clientId, clientSourceUrl, Grant.getBusesAsString(grants), scope,
                new Date(System.currentTimeMillis() + TokenPrivileged.EXPIRES_SECONDS * 1000));
        this.setGrants(grants);
    }

    @Override
    public String getChannelName() {
        return null;
    }

    public String getClientId() {
        return this.get(Field.ISSUED_TO_CLIENT_ID.getFieldName());
    }

    public List<Grant> getGrants() {
        return Collections.unmodifiableList(this.sourceGrants);
    }

    public void setGrants(List<Grant> grants) {
        this.sourceGrants.clear();
        for (Grant grant : grants) {
            addGrant(grant);
        }
    }

    public static enum Field implements MessageField {

        ISSUED_TO_CLIENT_ID("issued_to_client"),
        CLIENT_SOURCE_URL("client_source_url");

        @Override
        public String getFieldName() {
            return fieldName;
        }

        @Override
        public boolean isRequired() {
            return true;
        }

        @Override
        public void validate(String value) throws SimpleDBException {
            if (isRequired()) validateNotNull(getFieldName(), value);
        }

        // - PRIVATE

        private String fieldName;

        private Field(String fieldName) {
            this.fieldName = fieldName;
        }
    }

    private static final Logger logger = Logger.getLogger(TokenPrivileged.class);
    private List<Grant> sourceGrants = new ArrayList<Grant>();

    private void addGrant(Grant grant) {
        assert(grant.getGrantClientId().equals(this.getClientId()));
        this.sourceGrants.add(grant);
    }

    /**
     * Expand empty scope to all authorized buses.
     * Check that all buses in scopeString are contained in the authorized buses with which the token was created.
     */
    private String processScope(String scopeString) throws TokenException {
        String processedScopeString = scopeString;
        if (new Scope(processedScopeString).getBusesInScope().isEmpty()) {
            // if a privileged user has requested a token without specifying a bus in the scope, copy
            // over all authorized buses from the set of authorized buses

            if (StringUtils.isBlank(processedScopeString)) {
                processedScopeString = "";
            }

            processedScopeString = getEncodedBusesAsString() + " " + processedScopeString;
            this.setMustReturnScopeInResponse(true);
        }

        if ( ! isAllowedBuses(new Scope(processedScopeString).getBusesInScope()) ) {
            throw new TokenException(OAuth2.OAUTH2_TOKEN_INVALID_SCOPE, "Scope request not allowed");
        }

        logger.info("privileged token allowed scope:'" + processedScopeString + "' from auth'd buses:'" + getBusesAsString() + "'");

        return processedScopeString;
    }
}
