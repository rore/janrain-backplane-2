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

import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.commons.supersimpledb.message.AbstractMessage;
import com.janrain.commons.supersimpledb.message.MessageField;
import org.apache.commons.lang.StringUtils;

import java.net.MalformedURLException;
import java.net.URL;
import java.util.*;

/**
 * @author Johnny Bufu
 */
public class AuthorizationRequest extends AbstractMessage {

    // - PUBLIC

    /**
     * Empty default constructor for AWS to use
     */
    public AuthorizationRequest() { }

    public AuthorizationRequest(String cookie, String configuredRedirectUri, Map parameterMap) {
        Map<String,String> data = new LinkedHashMap<String, String>();

        for(Field f: EnumSet.allOf(Field.class)) {
            Object value = parameterMap.get(f.getFieldName().toLowerCase());
            if ( value != null && ((String[]) value).length > 0 && StringUtils.isNotEmpty(((String[])value)[0])) {
                data.put(f.getFieldName(), ((String[])value)[0]);
            }
        }

        data.put(Field.COOKIE.getFieldName(), cookie);
        data.put(Field.EXPIRES.getFieldName(), Backplane2Config.ISO8601.format(new Date(System.currentTimeMillis() + AUTH_REQUEST_TIMEOUT_SECONDS * 1000)));

        if(StringUtils.isEmpty(get(Field.REDIRECT_URI))) {
            data.put(Field.REDIRECT_URI.getFieldName(), configuredRedirectUri);
        }

        super.init(cookie, data);
    }

    @Override
    public String getIdValue() {
        return get(Field.COOKIE);
    }

    @Override
    public Set<? extends MessageField> getFields() {
        return EnumSet.allOf(Field.class);
    }

    public static enum Field implements MessageField {

        // - PUBLIC

        COOKIE,
        EXPIRES,
        CLIENT_ID,
        RESPONSE_TYPE {
            @Override
            public void validate(String value) throws RuntimeException {
                super.validate(value);
                if (! "authorization_code".equals(value)) {
                    throw new IllegalArgumentException("Unsupported OAuth2 response_type: " + value);
                }
            }
        },
        REDIRECT_URI {
            @Override
            public void validate(String value) throws RuntimeException {
                super.validate(value);
                try {
                    new URL(value);
                } catch (MalformedURLException e) {
                    throw new IllegalArgumentException("Invalid redirect_uri value: " + e.getMessage());
                }
            }
        },
        SCOPE(false),
        STATE(false);

        @Override
        public String getFieldName() {
            return name();
        }

        @Override
        public boolean isRequired() {
            return required;
        }

        @Override
        public void validate(String value) throws RuntimeException {
            if (isRequired()) validateNotNull(name(), value);
        }

        // - PRIVATE

        private final boolean required;

        private Field() {
            this(true);
        }

        private Field(boolean required) {
            this.required = required;
        }
    }

    // - PRIVATE

    private static final long AUTH_REQUEST_TIMEOUT_SECONDS = 1200l;
}
