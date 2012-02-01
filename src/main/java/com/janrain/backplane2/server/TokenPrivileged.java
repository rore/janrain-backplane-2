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

import com.janrain.commons.supersimpledb.message.MessageField;
import com.janrain.crypto.ChannelUtil;
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

    /**
     * Empty default constructor for AWS to use.
     */
    public TokenPrivileged() {}

    public TokenPrivileged(String tokenString, String clientId, String buses, String scopeString, Date expires) throws BackplaneServerException {
        super(tokenString, TYPE.PRIVILEGED_TOKEN, buses, scopeString, expires);

        assert(StringUtils.isNotEmpty(clientId));
        put(Field.ISSUED_TO_CLIENT_ID.getFieldName(), clientId);

    }

    public TokenPrivileged(String clientId, Grant grant, String scope) throws BackplaneServerException {
        this(ChannelUtil.randomString(TOKEN_LENGTH), clientId, grant.getBusesAsString(), scope, null);
        this.addGrant(grant);
    }

    public TokenPrivileged(String clientId, List<Grant> grants, String scope) throws BackplaneServerException {
        this(ChannelUtil.randomString(TOKEN_LENGTH), clientId, Grant.getBusesAsString(grants), scope, null);
        this.setGrants(grants);
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

        ISSUED_TO_CLIENT_ID("issued_to_client", true);

        @Override
        public String getFieldName() {
            return fieldName;
        }

        @Override
        public boolean isRequired() {
            return required;
        }

        @Override
        public void validate(String value) throws RuntimeException {
            if (isRequired()) validateNotNull(getFieldName(), value);
        }

        // - PRIVATE

        private String fieldName;
        private boolean required = true;

        private Field(String fieldName) {
            this(fieldName, true);
        }

        private Field(String fieldName, boolean required) {
            this.fieldName = fieldName;
            this.required = required;
        }
    }

    private static final Logger logger = Logger.getLogger(TokenPrivileged.class);
    private List<Grant> sourceGrants = new ArrayList<Grant>();

    private void addGrant(Grant grant) {
        assert(grant.getGrantClientId().equals(this.getClientId()));
        this.sourceGrants.add(grant);
    }
}
