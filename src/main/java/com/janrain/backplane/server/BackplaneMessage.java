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

package com.janrain.backplane.server;

import com.janrain.backplane.server.config.Backplane1Config;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.message.AbstractMessage;
import com.janrain.commons.supersimpledb.message.MessageField;
import com.janrain.commons.util.Pair;
import com.janrain.crypto.ChannelUtil;
import org.apache.log4j.Logger;
import org.codehaus.jackson.map.ObjectMapper;

import java.io.*;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.*;

/**
 * @author Johnny Bufu
 */
public final class BackplaneMessage extends AbstractMessage implements Serializable {

    // - PUBLIC

    /** For AWS use only */
    public BackplaneMessage() { }

    public BackplaneMessage(boolean generateNewId, String bus, String channel, Map<String, Object> data) throws BackplaneServerException, SimpleDBException {
        Map<String,String> d = new LinkedHashMap<String, String>(toStringMap(data));
        if (generateNewId) {
            d.put(Field.ID.getFieldName(), generateMessageId(new Date()));
        }
        d.put(Field.BUS.getFieldName(), bus);
        d.put(Field.CHANNEL_NAME.getFieldName(), channel);
        d.put(Field.PAYLOAD.getFieldName(), extractFieldValueAsJsonString(Field.PAYLOAD, data));
        if (! d.containsKey(Field.STICKY.getFieldName())) {
            d.put(Field.STICKY.getFieldName(), Boolean.FALSE.toString());
        }
        super.init(d.get(BackplaneMessage.Field.ID.getFieldName()), d);
    }

    @Override
    public String getIdValue() {
        return get(Field.ID);
    }

    @Override
    public Set<? extends MessageField> getFields() {
        return EnumSet.allOf(Field.class);
    }

    public HashMap<String, Object> asFrame() throws BackplaneServerException {

        HashMap<String, Object> frame = new LinkedHashMap<String, Object>();

        frame.put(Field.ID.getFieldName(), get(BackplaneMessage.Field.ID.getFieldName()));
        frame.put(Field.CHANNEL_NAME.getFieldName(), get(BackplaneMessage.Field.CHANNEL_NAME.getFieldName()));

        // don't copy to frame ID, BUS, CHANNEL_NAME verbatim from Message data structure
        // (and any other, non-Message fields, like ssdb_update_version)
        Map <String,Object> msg = new LinkedHashMap<String, Object>();

        // only add SOURCE, TYPE, STICKY, PAYLOAD to 'message: {...}'
        msg.put(Field.SOURCE.getFieldName(), get(Field.SOURCE));
        msg.put(Field.TYPE.getFieldName(), get(Field.TYPE));
        String sticky = get(Field.STICKY.getFieldName());
        if (sticky != null) {
            // print sticky as a (json) boolean
            msg.put(Field.STICKY.getFieldName(), Boolean.valueOf(sticky));
        }
        try {
            msg.put(
                BackplaneMessage.Field.PAYLOAD.getFieldName(),
                (new ObjectMapper()).readValue(get(BackplaneMessage.Field.PAYLOAD), Object.class) ); // un-quote the value
        } catch (IOException e) {
            String errMsg = "Error deserializing message payload: " + e.getMessage();
            logger.error(errMsg);
            throw new BackplaneServerException(errMsg, e);
        }
        frame.put("message", msg);

        return frame;
    }

    public static Date parseIdDate(String idValue) {
        try {
            return Backplane1Config.ISO8601.get().parse(idValue.substring(0, idValue.indexOf("Z") + 1));
        } catch (Exception e) {
            logger.error("Invalid ID value: " + idValue);
            return null;
        }
    }

    public Pair<String, Date> updateId(Pair<String, Date> lastIdAndDate) throws SimpleDBException {
        String id = get(Field.ID);
        if (id.compareTo(lastIdAndDate.getLeft()) <= 0) {
            logger.warn("message has an id " + id + " that is not > the latest id of " + lastIdAndDate.getLeft());
            Date newDate = new Date(lastIdAndDate.getRight().getTime() + 1);
            id = generateMessageId(newDate);
            put(Field.ID.getFieldName(), id);
            Field.ID.validate(id);
            logger.warn("fixed");
            return new Pair<String, Date>(id, newDate);
        } else {
            return lastIdAndDate;
        }
    }

    public Date getDate() {
        return parseIdDate(getIdValue());
    }

    public static enum Field implements MessageField {
        ID("id") {
            @Override
            public void validate(String value) throws SimpleDBException {
                super.validate(value);
                if (parseIdDate(value) == null) {
                    throw new IllegalArgumentException("Invalid value for " + getFieldName() + ": " + value);
                }
            }
        },
        CHANNEL_NAME("channel_name"),
        BUS("bus"),
        STICKY("sticky", false) {
            @Override
            public void validate(String value) throws SimpleDBException {
                super.validate(value);
                if (value != null && ! Boolean.TRUE.toString().equalsIgnoreCase(value) && ! Boolean.FALSE.toString().equalsIgnoreCase(value)) {
                    throw new IllegalArgumentException("Invalid boolean value for " + getFieldName() + ": " + value);
                }
            }},
        SOURCE("source") {
            @Override
            public void validate(String value) throws SimpleDBException {
                super.validate(value);
                try {
                    new URL(value);
                } catch (MalformedURLException e) {
                    throw new IllegalArgumentException("Invalid URL for " + getFieldName() + ": " + value, e);
                }
            }},

        TYPE("type"),
        PAYLOAD("payload");

        @Override
        public String getFieldName() {
            return fieldName;
        }

        @Override
        public boolean isRequired() {
            return required;
        }

        @Override
        public void validate(String value) throws SimpleDBException {
            if (isRequired()) validateNotBlank(getFieldName(), value);
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

    // - PRIVATE

    private static final Logger logger = Logger.getLogger(BackplaneMessage.class);

    /**
     * @return a time-based, lexicographically comparable message ID.
     */
    private static String generateMessageId(Date date) {
        return Backplane1Config.ISO8601.get().format(date) + "-" + ChannelUtil.randomString(10);
    }

    private String extractFieldValueAsJsonString(Field field, Map<String,Object> data) throws BackplaneServerException {
        try {
            Object value = data.get(field.getFieldName());
            return value == null ? null : (new ObjectMapper()).writeValueAsString(value);
        } catch (IOException e) {
            String errMsg = "Error serializing message payload: " + e.getMessage();
            logger.error(errMsg);
            throw new BackplaneServerException(errMsg, e);
        }
    }

    private Object writeReplace() {
        return new SerializationProxy(this);
    }

    private void readObject(ObjectInputStream stream) throws InvalidObjectException {
        throw new InvalidObjectException("Proxy required");
    }

    /** Class representing the logical serialization format for a backplane v1 message */
    private static class SerializationProxy implements Serializable {

        public SerializationProxy(BackplaneMessage message) {
            data.putAll(message);
        }

        private static final long serialVersionUID = 1958933880821698459L;

        // data HashMap is all we need
        private final HashMap<String,Object> data = new HashMap<String, Object>();

        private Object readResolve() throws ObjectStreamException {
            Object bus = data.get(BackplaneMessage.Field.BUS.getFieldName());
            Object channel = data.get(BackplaneMessage.Field.CHANNEL_NAME.getFieldName());
            // use public constructor
            try {
                return new BackplaneMessage(false, bus != null ? bus.toString() : null, channel != null ? channel.toString() : null, data);
            } catch (Exception e) {
                logger.error("Error deserializing message", e);
                throw new InvalidObjectException(e.getMessage());
            }
        }
    }
}