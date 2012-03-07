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
import com.janrain.commons.supersimpledb.message.AbstractMessage;
import com.janrain.commons.supersimpledb.message.MessageField;
import com.janrain.crypto.ChannelUtil;
import org.apache.log4j.Logger;
import org.codehaus.jackson.map.ObjectMapper;
import java.io.IOException;
import java.net.MalformedURLException;
import java.net.URL;
import java.util.*;

/**
 * @author Johnny Bufu
 */
public class BackplaneMessage extends AbstractMessage {

    // - PUBLIC

    @SuppressWarnings("UnusedDeclaration")
    public BackplaneMessage() {}

    public BackplaneMessage(String clientSourceUrl, Map<String, Object> data) throws BackplaneServerException {
        if (data.containsKey(Field.SOURCE.getFieldName())) {
            throw new IllegalArgumentException("Upstream messages must not include the 'source' field.");
        }
        Map<String,String> d = new LinkedHashMap<String, String>(toStringMap(data));
        String id = generateMessageId();
        d.put(Field.ID.getFieldName(), id);
        d.put(Field.TYPE.getFieldName(), data.get(Field.TYPE.getFieldName()).toString());
        d.put(Field.SOURCE.getFieldName(), clientSourceUrl);
        d.put(Field.BUS.getFieldName(), data.get(Field.BUS.getFieldName()).toString());
        d.put(Field.CHANNEL.getFieldName(), data.get(Field.CHANNEL.getFieldName()).toString());
        d.put(Field.PAYLOAD.getFieldName(), extractFieldValueAsJsonString(Field.PAYLOAD, data));
        if (! d.containsKey(Field.STICKY.getFieldName())) {
            d.put(Field.STICKY.getFieldName(), Boolean.FALSE.toString());
        }

        if (getFields().size() < d.size()) {
            throw new IllegalArgumentException("Extra invalid parameter(s)");
        }

        super.init(id, d);
    }

    /**
     * @return a time-based, lexicographically comparable message ID.
     */
    public static String generateMessageId() {
        return Backplane2Config.ISO8601.format(new Date()) + "-" + ChannelUtil.randomString(10);
    }

    @Override
    public String getIdValue() {
        return get(Field.ID);
    }

    @Override
    public Set<? extends MessageField> getFields() {
        return EnumSet.allOf(Field.class);
    }

    public Map<String, Object> asFrame(String serverDomain, boolean includePayload) throws BackplaneServerException {

        HashMap<String, Object> frame = new LinkedHashMap<String, Object>();

        String serverUrl = "https://" + serverDomain + "/v2/message";

        frame.put("messageURL", serverUrl + "/" + get(Field.ID));
        frame.put(Field.SOURCE.getFieldName(), get(Field.SOURCE));
        frame.put(Field.TYPE.getFieldName(), get(Field.TYPE));
        frame.put(Field.BUS.getFieldName(), get(Field.BUS));
        frame.put(Field.CHANNEL.getFieldName(), get(Field.CHANNEL));
        frame.put(Field.STICKY.getFieldName(), containsKey(Field.STICKY.getFieldName()) ? get(Field.STICKY) : Boolean.FALSE.toString());

        try {
            if (includePayload) {
                frame.put(Field.PAYLOAD.getFieldName(), (new ObjectMapper()).readValue(get(Field.PAYLOAD), Object.class) ); // un-quote the value)
            }
        } catch (IOException e) {
            String errMsg = "Error deserializing message payload: " + e.getMessage();
            logger.error(errMsg);
            throw new BackplaneServerException(errMsg, e);
        }

        /*  sample response does not include sticky field?  It should, right?
        String sticky = get(Field.STICKY.getFieldName());
        if (sticky != null) {
            // print sticky as a (json) boolean
            frame.put(Field.STICKY.getFieldName(), Boolean.valueOf(sticky));
        }
        */

        return frame;
    }

    public static enum Field implements MessageField {
        ID("id"),
        CHANNEL("channel"),
        BUS("bus"),
        STICKY("sticky", false) {
            @Override
            public void validate(String value) throws RuntimeException {
                super.validate(value);
                if (value != null && ! Boolean.TRUE.toString().equalsIgnoreCase(value) && ! Boolean.FALSE.toString().equalsIgnoreCase(value)) {
                    throw new IllegalArgumentException("Invalid boolean value for " + getFieldName() + ": " + value);
                }
            }},
        SOURCE("source") {
            @Override
            public void validate(String value) throws RuntimeException {
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
        public void validate(String value) throws RuntimeException {
            if (isRequired()) validateNotNull(getFieldName(), value);
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

}
