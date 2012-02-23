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
import com.janrain.oauth2.TokenException;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;

import java.util.Date;

/**
 * @author Tom Raney
 */

public abstract class Token extends Base {

    public static final int TOKEN_LENGTH = 20;
    public static final int EXPIRES_SECONDS = 3600;
    public static final String ANONYMOUS = "anonymous";
    public static enum TYPE {REGULAR_TOKEN, PRIVILEGED_TOKEN}

    /**
     * Empty default constructor for AWS to use.
     * Don't call directly.
     */
    public Token() {}

    /**
     * Token constructor
     * @param tokenString required
     * @param accessType  REGULAR_TOKEN or PRIVILEGED_TOKEN
     * @param buses       optional
     * @param scopeString optional
     * @param expires     if null, token does not expire
     * @throws BackplaneServerException
     */
    Token(String tokenString, TYPE accessType, String buses, String scopeString, Date expires) throws TokenException {
        super(tokenString,buses,expires);

        logger.debug("creating token with id '" + tokenString + "'");
        assert( (accessType == TYPE.REGULAR_TOKEN && tokenString.startsWith("an")) ||
                (accessType == TYPE.PRIVILEGED_TOKEN && tokenString.startsWith("pr")));

        put(TokenField.TYPE.getFieldName(), accessType.name());

        setScopeString(scopeString);

        validate();

    }

    public String getTokenType() {
        return "Bearer";
    }

    public TYPE getAccessType() {
        return TYPE.valueOf(this.get(TokenField.TYPE));
    }

    public Scope getScope() throws TokenException {
        return new Scope(this.get(TokenField.SCOPE));
    }

    public boolean isPrivileged() {
        return getAccessType() == TYPE.PRIVILEGED_TOKEN;
    }

    public String getScopeString() {
        return get(TokenField.SCOPE);
    }

    public void setScopeString(String scopeString) {
        if (StringUtils.isBlank(scopeString)) {
            return;
        }
        scopeString = scopeString.trim();
        logger.debug("new scope string: '" + scopeString + "'");
        put(TokenField.SCOPE.getFieldName(), scopeString);
    }

    public void setMustReturnScopeInResponse(boolean flag) {
        this.mustReturnScopeInResponse = flag;
    }

    public boolean mustReturnScopeInResponse() {
        return this.mustReturnScopeInResponse;
    }

    public abstract String getChannelName();


    public static enum TokenField implements MessageField {
        TYPE("type", true),
        SCOPE("scope", false);


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

        private TokenField(String fieldName, boolean required) {
            this.fieldName = fieldName;
            this.required = required;
        }
    }

    private static final Logger logger = Logger.getLogger(Token.class);
    private boolean mustReturnScopeInResponse = false;


}
