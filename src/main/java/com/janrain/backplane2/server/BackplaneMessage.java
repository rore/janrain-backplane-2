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

import com.janrain.backplane.server.ExternalizableCore;
import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.message.MessageField;
import com.janrain.commons.util.Pair;
import com.janrain.crypto.ChannelUtil;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.codehaus.jackson.map.ObjectMapper;

import java.io.IOException;
import java.text.ParseException;
import java.util.*;

import static com.janrain.backplane2.server.Scope.ScopeType.*;

/**
 * @author Johnny Bufu
 */
public class BackplaneMessage extends ExternalizableCore {

    // - PUBLIC

    @SuppressWarnings("UnusedDeclaration")
    public BackplaneMessage() {}

    public BackplaneMessage(String clientSourceUrl, Map<String, Object> data) throws BackplaneServerException, SimpleDBException {
        if (data.containsKey(Field.SOURCE.getFieldName())) {
            throw new InvalidRequestException("Upstream messages must not include the 'source' field.");
        }
        Map<String,String> d = new LinkedHashMap<String, String>(toStringMap(data));
        String id = generateMessageId(new Date());
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
            throw new InvalidRequestException("Extra invalid parameter(s)");
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

    public Pair<String, Date> updateId(Pair<String, Date> lastIdAndDate) {
        long thisTime = getDateFromId(getIdValue()).getTime();
        long lastTime = getDateFromId(lastIdAndDate.getLeft()).getTime();

        if (thisTime <= lastTime) {
            logger.warn("message has an id " + getIdValue() + " that is not > the latest id of " + lastIdAndDate.getLeft());
            Date newDate = new Date(lastIdAndDate.getRight().getTime() + 1);
            put(Field.ID.getFieldName(), generateMessageId(newDate));
            logger.warn("fixed");
            return new Pair<String, Date>(getIdValue(), newDate);
        } else {
            return lastIdAndDate;
        }
    }

    public String getBus() {
        return this.get(Field.BUS);
    }

    public String getChannel() {
        return this.get(Field.CHANNEL);
    }

    public boolean isSticky() {
        return "true".equalsIgnoreCase(get(Field.STICKY));
    }

    public Map<String, Object> asFrame(String serverDomain, boolean includePayload) throws BackplaneServerException {

        HashMap<String, Object> frame = new LinkedHashMap<String, Object>();

        String serverUrl = "https://" + serverDomain + "/v2/message";
        frame.put(Field.MESSAGE_URL.getFieldName(), serverUrl + "/" + get(Field.ID));
        frame.put(Field.SOURCE.getFieldName(), get(Field.SOURCE));
        frame.put(Field.TYPE.getFieldName(), get(Field.TYPE));
        frame.put(Field.BUS.getFieldName(), get(Field.BUS));
        frame.put(Field.CHANNEL.getFieldName(), get(Field.CHANNEL));
        frame.put(Field.STICKY.getFieldName(), containsKey(Field.STICKY.getFieldName()) ? get(Field.STICKY) : Boolean.FALSE.toString());

        try {
            if (includePayload) {
                frame.put(Field.PAYLOAD.getFieldName(), (new ObjectMapper()).readValue(get(Field.PAYLOAD), Object.class) ); // un-quote the value
            }
        } catch (IOException e) {
            String errMsg = "Error deserializing message payload: " + e.getMessage();
            logger.error(errMsg);
            throw new BackplaneServerException(errMsg, e);
        }

        return frame;
    }

    public static enum Field implements MessageField {

        ID("id", NONE),

        CHANNEL("channel", FILTER),

        BUS("bus", AUTHZ_REQ),

        STICKY("sticky", false, FILTER) {
            @Override
            public void validate(String value) throws SimpleDBException {
                super.validate(value);
                if (value != null && ! Boolean.TRUE.toString().equalsIgnoreCase(value) && ! Boolean.FALSE.toString().equalsIgnoreCase(value)) {
                    throw new InvalidRequestException("Invalid boolean value for " + getFieldName() + ": " + value);
                }
            }},

        SOURCE("source", FILTER) {
            @Override
            public void validate(String value) throws SimpleDBException {
                super.validate(value);
                validateUrl(getFieldName(), value);
            }},

        TYPE("type", FILTER),

        MESSAGE_URL("messageURL", false, FILTER) {
            @Override
            public void validate(String value) throws SimpleDBException {
                super.validate(value);
                if (StringUtils.isNotEmpty(value)) {
                    validateUrl(getFieldName(), value);
                }
            }
        },

        PAYLOAD("payload", NONE);

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

        public Scope.ScopeType getScopeType() {
            return scopeType;
        }

        // - PRIVATE

        private String fieldName;
        private final boolean required;
        private final Scope.ScopeType scopeType;

        private Field(String fieldName, Scope.ScopeType scopeType) {
            this(fieldName, true, scopeType);
        }

        private Field(String fieldName, boolean required, Scope.ScopeType scopeType) {
            this.fieldName = fieldName;
            this.required = required;
            this.scopeType = scopeType;
        }
    }

    public static Date getDateFromId(String backplaneMessageId) {
        if (backplaneMessageId == null) {
            return null;
        }

        try {
            return Backplane2Config.ISO8601.get().parse(backplaneMessageId.substring(0, backplaneMessageId.indexOf("Z") + 1));
        } catch (ParseException e) {
            logger.warn(e);
        }
        return null;
    }

    // - PRIVATE

    private static final long serialVersionUID = -6609794896611874473L;

    private static final Logger logger = Logger.getLogger(BackplaneMessage.class);

    /**
     * @return a time-based, lexicographically comparable message ID.
     */
    private static String generateMessageId(Date date) {
        return Backplane2Config.ISO8601.get().format(date) + "-" + ChannelUtil.randomString(10);
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

}
