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

import com.janrain.backplane2.server.dao.DaoFactory;
import com.janrain.backplane2.server.provision.ProvisioningConfig;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.message.MessageField;
import org.apache.log4j.Logger;

import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.ObjectStreamException;
import java.io.Serializable;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;
import java.util.Set;

/**
 * @author Johnny Bufu
 */
public final class User extends ProvisioningConfig implements Serializable {

    // - PUBLIC

    /**
     * Empty default constructor for AWS to use
     */
    public User() {}

    public User(String userName, String pwdHash) {
        put(Field.USER.getFieldName(), userName);
        put(Field.PWDHASH.getFieldName(), pwdHash);
    }

    @Override
    public String getIdValue() {
        return get(Field.USER);
    }

    @Override
    public Set<? extends MessageField> getFields() {
        return EnumSet.allOf(Field.class);
    }

    @Override
    public void validate(DaoFactory daoFactory) throws Exception {
        validate();
    }

    public static enum Field implements MessageField {

        // - PUBLIC

        USER,
        PWDHASH;

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

    private static final Logger logger = Logger.getLogger(User.class);

    private User(Map<String, String> data) throws SimpleDBException {
        super.init(data.get(Field.USER.getFieldName()), data);
    }

    private Object writeReplace() {
        return new SerializationProxy(this);
    }

    private void readObject(ObjectInputStream stream) throws InvalidObjectException {
        throw new InvalidObjectException("Proxy required");
    }

    /** Class representing the logical serialization format for a backplane v2 bus owner */
    private static class SerializationProxy implements Serializable {

        public SerializationProxy(User user) {
            data.putAll(user);
        }

        private static final long serialVersionUID = 4751544645113124933L;

        // data HashMap is all we need
        private final HashMap<String,String> data = new HashMap<String, String>();

        private Object readResolve() throws ObjectStreamException {
            try {
                return new User(data);
            } catch (Exception e) {
                logger.error("Error deserializing user", e);
                throw new InvalidObjectException(e.getMessage());
            }
        }
    }
}
