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

import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.message.AbstractMessage;
import com.janrain.commons.supersimpledb.message.MessageField;
import org.apache.log4j.Logger;
import org.codehaus.jackson.map.ObjectMapper;

import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.io.Serializable;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.*;

/**
 * @author Johnny Bufu
 */
public class BackplaneMessage extends AbstractMessage implements Serializable {

    // - PUBLIC

    public BackplaneMessage(String id, String bus, String channel, Map<String, Object> data) throws BackplaneServerException, SimpleDBException {
        Map<String,String> d = new LinkedHashMap<String, String>(toStringMap(data));
        d.put(Field.ID.getFieldName(), id);
        d.put(Field.BUS.getFieldName(), bus);
        d.put(Field.CHANNEL_NAME.getFieldName(), channel);
        d.put(Field.PAYLOAD.getFieldName(), extractFieldValueAsJsonString(Field.PAYLOAD, data));
        if (! d.containsKey(Field.STICKY.getFieldName())) {
            d.put(Field.STICKY.getFieldName(), Boolean.FALSE.toString());
        }
        super.init(id, d);
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

    public static enum Field implements MessageField {
        ID("id"),
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



    // - PACKAGE

    public BackplaneMessage() {
    }

    // - PRIVATE

    private static final Logger logger = Logger.getLogger(BackplaneMessage.class);

    private static final long serialVersionUID = 7526471155622776147L;

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


    private void writeObject(ObjectOutputStream oos)
            throws IOException {

        Map<String, String> map = new HashMap<String,String>();
        for (Map.Entry<String,String> entry : this.entrySet()) {
            map.put(entry.getKey(), entry.getValue());
        }
        oos.writeObject(map);

    }

    private void readObject(ObjectInputStream ois)
            throws ClassNotFoundException, IOException {

        Map<String,String> map = (Map<String, String>) ois.readObject();
        try {
            init(map.get(Field.ID.getFieldName()), map);
        } catch (SimpleDBException e) {
            logger.error(e);
        }
    }

}
