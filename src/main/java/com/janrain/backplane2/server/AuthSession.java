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

import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.message.AbstractMessage;
import com.janrain.commons.supersimpledb.message.MessageField;
import org.apache.log4j.Logger;

import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.ObjectStreamException;
import java.io.Serializable;
import java.util.*;

/**
 * @author Johnny Bufu
 */
public final class AuthSession extends AbstractMessage implements Serializable {

    // - PUBLIC

    /**
     * Empty default constructor for AWS to use
     */
    public AuthSession() { }
    
    public AuthSession(String authUser, String cookie) throws SimpleDBException {
        Map<String,String> data = new LinkedHashMap<String, String>();
        data.put(Field.AUTH_USER.getFieldName(), authUser);
        data.put(Field.COOKIE.getFieldName(), cookie);
        data.put(Field.EXPIRES.getFieldName(), Backplane2Config.ISO8601.get().format(new Date(System.currentTimeMillis() + AUTH_SESSION_TIMEOUT_SECONDS * 1000)));
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
        AUTH_USER,
        EXPIRES;

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

    private static final long AUTH_SESSION_TIMEOUT_SECONDS = 3600l;

    private static final Logger logger = Logger.getLogger(AuthSession.class);

    private AuthSession(Map<String, String> data) throws SimpleDBException {
        super.init(data.get(Field.COOKIE.getFieldName()), data);
    }

    private Object writeReplace() {
        return new SerializationProxy(this);
    }

    private void readObject(ObjectInputStream stream) throws InvalidObjectException {
        throw new InvalidObjectException("Proxy required");
    }

    /** Class representing the logical serialization format for a backplane v2 authentication session */
    private static class SerializationProxy implements Serializable {

        public SerializationProxy(AuthSession authSession) {
            data.putAll(authSession);
        }

        private static final long serialVersionUID = 1172887278719701898L;

        // data HashMap is all we need
        private final HashMap<String,String> data = new HashMap<String, String>();

        private Object readResolve() throws ObjectStreamException {
            try {
                return new AuthSession(data);
            } catch (Exception e) {
                logger.error("Error deserializing bus auth session", e);
                throw new InvalidObjectException(e.getMessage());
            }
        }
    }
}
