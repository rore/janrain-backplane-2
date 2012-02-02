/*
 * Copyright 2011 Janrain, Inc.
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

import java.util.Date;

/**
 * @author Tom Raney
 */
public class TokenAnonymous extends Token {

    /**
     * Empty default constructor for AWS to use.
     */
    public TokenAnonymous() {}

    public TokenAnonymous(String tokenString, String buses, String scopeString, Date expires) throws BackplaneServerException {
        super("an" + tokenString, TYPE.REGULAR_TOKEN, buses, scopeString, expires);

        // verify that no channel or bus was submitted in the scopeString request
        Scope testScope = new Scope(scopeString);
        if (!testScope.getBusesInScope().isEmpty() || !testScope.getChannelsInScope().isEmpty()) {
            throw new BackplaneServerException("Scope request not allowed for regular token");
        }

        String channel = ChannelUtil.randomString(CHANNEL_NAME_LENGTH);
        put(Field.CHANNEL.getFieldName(), channel);
        // set the scope string to include this new channel
        if (StringUtils.isEmpty(scopeString)) {
            scopeString = "channel:" + channel;
        }  else {
            scopeString += " channel:" + channel;
        }

        setScopeString(scopeString);

        validate();
    }

    public TokenAnonymous(String buses, String scopeString, Date expires) throws BackplaneServerException {
        this(ChannelUtil.randomString(TOKEN_LENGTH), buses, scopeString, expires);
    }

    @Override
    public String getChannelName() {
        return this.get(Field.CHANNEL);
    }

    public static enum Field implements MessageField {

        CHANNEL("channel", true);


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

}
