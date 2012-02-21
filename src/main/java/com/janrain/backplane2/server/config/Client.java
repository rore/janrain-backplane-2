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

package com.janrain.backplane2.server.config;

import com.janrain.backplane2.server.Token;
import com.janrain.commons.supersimpledb.message.MessageField;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * @author Tom Raney
 */
public class Client extends User {

    public static final Client ANONYMOUS_CLIENT = new Client() {{ // bypass regular field validation
        put(Field.USER.getFieldName(), Token.ANONYMOUS);
    }};
    
    /**
     * Empty default constructor for AWS to use
     */
    public Client() {}

    public Client(String client_id, String client_secret, String source_url, String redirect_uri) {
        Map<String,String> d = new LinkedHashMap<String, String>();
        d.put(Field.USER.getFieldName(), client_id);
        d.put(Field.PWDHASH.getFieldName(), client_secret);
        d.put(ClientField.SOURCE_URL.getFieldName(), source_url);
        d.put(ClientField.REDIRECT_URI.getFieldName(), redirect_uri);
        super.init(client_id, d);
    }

    public String getClientId() {
        return this.get(Field.USER);
    }

    public String getClientSecret() {
        return this.get(Field.PWDHASH);
    }

    public String getRedirectUri() {
        return this.get(ClientField.REDIRECT_URI);
    }

    public static enum ClientField implements MessageField {

        // - PUBLIC

        SOURCE_URL,
        
        REDIRECT_URI;

        @Override
        public String getFieldName() {
            return name();
        }

        @Override
        public boolean isRequired() {
            return true;
        }

        @Override
        public void validate(String value) throws RuntimeException {
            if (isRequired()) validateNotNull(name(), value);
        }
    }
}
