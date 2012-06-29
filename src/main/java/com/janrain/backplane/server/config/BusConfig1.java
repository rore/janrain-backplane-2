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

package com.janrain.backplane.server.config;

import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.message.AbstractMessage;
import com.janrain.commons.supersimpledb.message.MessageField;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;

import java.io.InvalidObjectException;
import java.io.ObjectInputStream;
import java.io.ObjectStreamException;
import java.io.Serializable;
import java.util.*;

/**
 * @author Johnny Bufu
 */
public final class BusConfig1 extends AbstractMessage implements Serializable {

    // - PUBLIC

    /** For AWS use only */
    public BusConfig1() { }

    public BusConfig1(HashMap<String, Object> data) throws SimpleDBException {
        Map<String,String> d = new LinkedHashMap<String, String>(toStringMap(data));
        super.init(d.get(Field.BUS_NAME.getFieldName()), d);
    }

    @Override
    public String getIdValue() {
        return get(Field.BUS_NAME);
    }

    @Override
    public Set<? extends MessageField> getFields() {
        return EnumSet.allOf(Field.class);
    }

    public Collection<Backplane1Config.BUS_PERMISSION> getPermissions(String user) {
        if (isBusConfigField(user)) {
            throw new IllegalArgumentException("Invalid user name: " + user);
        }

        String perms = get(user);
        EnumSet<Backplane1Config.BUS_PERMISSION> result = EnumSet.noneOf(Backplane1Config.BUS_PERMISSION.class);
        if (StringUtils.isNotBlank(perms)) {
            for(String perm : perms.split(",")) {
                result.add(Backplane1Config.BUS_PERMISSION.valueOf(perm));
            }
        }
        return result;
    }

    public static enum Field implements MessageField {

        BUS_NAME,

        RETENTION_TIME_SECONDS {
            @Override
            public void validate(String value) throws SimpleDBException {
                if (isRequired() || value != null) {
                    String fieldName = getFieldName();
                    int intValue = validateInt(fieldName, value);
                    if (intValue < RETENTION_MIN_SECONDS || intValue > RETENTION_MAX_SECONDS) {
                        throw new IllegalArgumentException("Value of " + fieldName + " = " + intValue + " but must be between " + RETENTION_MIN_SECONDS + " and " + RETENTION_MAX_SECONDS);
                    }
                }
            }},

        RETENTION_STICKY_TIME_SECONDS {
            @Override
            public void validate(String value) throws SimpleDBException {
                if (isRequired() || value != null) {
                    String fieldName = getFieldName();
                    validateInt(fieldName, value);
                    int intValue = validateInt(fieldName, value);
                    if (intValue < RETENTION_STICKY_MIN_SECONDS || intValue > RETENTION_STICKY_MAX_SECONDS) {
                        throw new IllegalArgumentException("Value of " + fieldName + " = " + intValue + " but must be between " + RETENTION_STICKY_MIN_SECONDS + " and " + RETENTION_STICKY_MAX_SECONDS);
                    }
                }
            }};




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

        // - PRIVATE

        private static final int RETENTION_MIN_SECONDS = 60;
        private static final int RETENTION_MAX_SECONDS = 604800; // one week
        private static final int RETENTION_STICKY_MIN_SECONDS = 28800; // eight hours
        private static final int RETENTION_STICKY_MAX_SECONDS = 604800; // one week

    }

    // - PRIVATE

    private static final Logger logger = Logger.getLogger(BusConfig1.class);

    private boolean isBusConfigField(String name) {
        try {
            Field.valueOf(name);
            return true;
        } catch (IllegalArgumentException e) {
            return false;
        }
    }

    private Object writeReplace() {
        return new SerializationProxy(this);
    }

    private void readObject(ObjectInputStream stream) throws InvalidObjectException {
        throw new InvalidObjectException("Proxy required");
    }

    /** Class representing the logical serialization format for a backplane v1 bus config */
    private static class SerializationProxy implements Serializable {

        public SerializationProxy(BusConfig1 busConfig) {
            data.putAll(busConfig);
        }

        private static final long serialVersionUID = 1632268909973084052L;

        // data HashMap is all we need
        // todo: consider a custom string serialization format, for easy editing directly in the DB
        private final HashMap<String,Object> data = new HashMap<String, Object>();

        private Object readResolve() throws ObjectStreamException {
            // use public constructor
            try {
                return new BusConfig1(data);
            } catch (Exception e) {
                logger.error("Error deserializign bus config", e);
                throw new InvalidObjectException(e.getMessage());
            }
        }
    }
}
