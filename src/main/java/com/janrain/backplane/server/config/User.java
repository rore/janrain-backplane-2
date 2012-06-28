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
import com.janrain.commons.util.IOUtils;
import org.apache.log4j.Logger;

import java.io.*;
import java.util.*;

/**
 * @author Johnny Bufu
 */
public final class User extends AbstractMessage implements Serializable {

    // - PUBLIC

    /** For AWS use only */
    public User() { }

    public User(HashMap<String, Object> data) throws SimpleDBException {
        Map<String,String> d = new LinkedHashMap<String, String>(toStringMap(data));
        super.init(d.get(Field.USER.getFieldName()), d);
    }

    @Override
    public String getIdValue() {
        return get(Field.USER);
    }

    @Override
    public Set<? extends MessageField> getFields() {
        return EnumSet.allOf(Field.class);
    }

    public byte[] toBytes() {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        ObjectOutputStream oos = null;
        try {
            oos = new ObjectOutputStream(bos);
            oos.writeObject(this);
            oos.flush();
            return bos.toByteArray();
        } catch (IOException e) {
            logger.error("Error serializing user config", e);
            return null;
        } finally {
            IOUtils.closeSilently(oos);
            IOUtils.closeSilently(bos);
        }
    }

    public static User fromBytes(byte[] bytes) {
        if (bytes == null) {
            return null;
        }

        ObjectInputStream in = null;
        try {
            in = new ObjectInputStream(new ByteArrayInputStream(bytes));
            return (User) in.readObject();
        } catch (Exception e) {
            logger.error("Error deserializign user config", e);
            return null;
        } finally {
            IOUtils.closeSilently(in);
        }
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

    private Object writeReplace() {
        return new SerializationProxy(this);
    }

    private void readObject(ObjectInputStream stream) throws InvalidObjectException {
        throw new InvalidObjectException("Proxy required");
    }

    /** Class representing the logical serialization format for a backplane v1 user */
    private static class SerializationProxy implements Serializable {

        public SerializationProxy(User user) {
            data.putAll(user);
        }

        private static final long serialVersionUID = 6026489226237987114L;

        // data HashMap is all we need
        // todo: consider a custom string serialization format, for easy editing directly in the DB
        private final HashMap<String,Object> data = new HashMap<String, Object>();

        private Object readResolve() throws ObjectStreamException {
            // use public constructor
            try {
                return new User(data);
            } catch (Exception e) {
                logger.error("Error deserializign message", e);
                throw new InvalidObjectException(e.getMessage());
            }
        }
    }

}
