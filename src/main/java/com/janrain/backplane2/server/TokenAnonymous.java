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

import com.janrain.backplane.server.ApplicationException;
import com.janrain.commons.supersimpledb.message.MessageField;
import com.janrain.commons.util.EncryptUtil;
import com.janrain.crypto.ChannelUtil;
import com.janrain.oauth2.OAuth2;
import com.janrain.oauth2.TokenException;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.codehaus.jackson.map.ObjectMapper;
import org.codehaus.jackson.type.TypeReference;

import java.util.Date;

/**
 * @author Tom Raney
 */
public class TokenAnonymous extends Token {

    public static final int CHANNEL_NAME_LENGTH = 32;
    public static final int EXPIRES_SECONDS = 3600;

    /**
     * Empty default constructor for AWS to use.
     */
    public TokenAnonymous() {}

    public TokenAnonymous(String tokenString, String buses, String scopeString, Date expires) throws TokenException {
        super(Token.AN + tokenString, TYPE.REGULAR_TOKEN, buses, scopeString, expires);

        //TODO: the expiration of anonymous tokens is a problem - one hour is too short and having
        // them never expire will also cause excessive buildup.

        // verify that no channel or bus was submitted in the scopeString request
        Scope testScope = new Scope(scopeString);
        if (!testScope.getBusesInScope().isEmpty() || !testScope.getChannelsInScope().isEmpty()) {
            throw new TokenException(OAuth2.OAUTH2_TOKEN_INVALID_SCOPE, "Scope request not allowed");
        }

        String channel = ChannelUtil.randomString(CHANNEL_NAME_LENGTH);
        put(Field.CHANNEL.getFieldName(), channel);
        // set the scope string to include this new channel
        if (StringUtils.isEmpty(scopeString)) {
            scopeString = "channel:" + channel;
        }  else {
            scopeString += " channel:" + channel;
        }

        setMustReturnScopeInResponse(true);
        setScopeString(scopeString);

        validate();
    }

    public TokenAnonymous(String buses, String scopeString, Date expires) throws TokenException {
        this(ChannelUtil.randomString(TOKEN_LENGTH), buses, scopeString, expires);
    }

    public void setEncryptionKey(String encryptionKey) {
        this.encryptionKey = encryptionKey;
    }

    /**
	 * Parse a String representation of a TokenAnonymous (the cookie value)
	 * that was created via the toString() method.
	 *
	 * @param cookieValue		The String to parse
	 * @return					A SessionCookieData object parsed from the String
	 */
	public static TokenAnonymous asTokenAnonymous(String cookieValue, String encryptionKey) {

		logger.debug("Parsing encrypted session data:\n" + cookieValue);
		String unencryptedValue = EncryptUtil.decrypt(cookieValue, encryptionKey);
		ObjectMapper mapper = new ObjectMapper();
		TokenAnonymous data;
		try {
			data = mapper.readValue(unencryptedValue, new TypeReference<TokenAnonymous>() { });
            data.setEncryptionKey(encryptionKey);
		}
		catch (Exception e) {
			logger.error("Error reading token data from JSON '" + unencryptedValue + "'", e);
			throw new ApplicationException("Unable to parse token data from cookie value", e);
		}
		return data;
	}

    /**
	 * Get the String representation of a TokenAnonymous (for use in
	 * storing as a cookie value). This can be converted back via the
	 * TokenAnonymous.asTokenAnonymous() method.
	 *
	 * @return A String representation of the TokenAnonymous
	 */
	public String toString() {

		ObjectMapper mapper = new ObjectMapper();
		String value;
		try {
			value = mapper.writeValueAsString(this);
		}
		catch (Exception e) {
			logger.error("Unable to TokenAnonymous object to JSON", e);
			value = "";
		}

		String encrypted = EncryptUtil.encrypt(value, encryptionKey);
		logger.debug("Sending encrypted TokenAnonymous:\n" + encrypted);
		return encrypted;
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

        private Field(String fieldName, boolean required) {
            this.fieldName = fieldName;
            this.required = required;
        }
    }

    private static final Logger logger = Logger.getLogger(TokenAnonymous.class);

    private String encryptionKey;

}
