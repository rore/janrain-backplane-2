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

import com.janrain.backplane2.server.InvalidRequestException;
import com.janrain.backplane2.server.dao.DaoFactory;
import com.janrain.backplane2.server.provision.ProvisioningConfig;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.message.MessageField;
import com.janrain.oauth2.OAuth2;
import com.janrain.oauth2.ValidationException;
import org.apache.log4j.Logger;

import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.ObjectStreamException;
import java.io.Serializable;
import java.util.*;

/**
 * @author Tom Raney
 */
public final class Client extends ProvisioningConfig implements Serializable {

    /**
     * Empty default constructor for AWS to use
     */
    public Client() {}

    public Client(String client_id, String client_secret, String source_url, String redirect_uri) throws SimpleDBException {
        Map<String,String> d = new LinkedHashMap<String, String>();
        d.put(ClientField.USER.getFieldName(), client_id);
        d.put(ClientField.PWDHASH.getFieldName(), client_secret);
        d.put(ClientField.SOURCE_URL.getFieldName(), source_url);
        d.put(ClientField.REDIRECT_URI.getFieldName(), redirect_uri);
        super.init(client_id, d);
    }

    @Override
    public String getIdValue() {
        return get(ClientField.USER);
    }

    @Override
    public Set<? extends MessageField> getFields() {
        return EnumSet.allOf(ClientField.class);
    }

    @Override
    public void validate(DaoFactory daoFactory) throws Exception {
        validate();
    }

    public String getClientId() {
        return this.get(ClientField.USER);
    }

    public String getClientSecret() {
        return this.get(ClientField.PWDHASH);
    }

    public String getRedirectUri() {
        return this.get(ClientField.REDIRECT_URI);
    }

    public String getSourceUrl() {
        return this.get(ClientField.SOURCE_URL);
    }

    public static enum ClientField implements MessageField {

        // - PUBLIC

        USER,
        PWDHASH,
        SOURCE_URL,
        REDIRECT_URI {
            @Override
            public void validate(String value) throws SimpleDBException {
                super.validate(value);
                try {
                    OAuth2.validateRedirectUri(value);
                } catch (ValidationException e) {
                    throw new InvalidRequestException(e.getMessage());
                }
            }
        };

        @Override
        public String getFieldName() {
            return name();
        }

        @Override
        public boolean isRequired() {
            return true;
        }

        @Override
        public void validate(String value) throws SimpleDBException {
            if (isRequired()) validateNotBlank(name(), value);
        }
    }

    // - PRIVATE

    private static final Logger logger = Logger.getLogger(Client.class);

    private Client(Map<String, String> data) throws SimpleDBException {
        super.init(data.get(ClientField.USER.getFieldName()), data);
    }

    private Object writeReplace() {
        return new SerializationProxy(this);
    }

    private void readObject(ObjectInputStream stream) throws InvalidObjectException {
        throw new InvalidObjectException("Proxy required");
    }

    /** Class representing the logical serialization format for a backplane v2 client */
    private static class SerializationProxy implements Serializable {

        public SerializationProxy(Client client) {
            data.putAll(client);
        }

        private static final long serialVersionUID = 8174767390986029178L;

        // data HashMap is all we need
        private final HashMap<String,String> data = new HashMap<String, String>();

        private Object readResolve() throws ObjectStreamException {
            try {
                return new Client(data);
            } catch (Exception e) {
                logger.error("Error deserializing client", e);
                throw new InvalidObjectException(e.getMessage());
            }
        }
    }
}
