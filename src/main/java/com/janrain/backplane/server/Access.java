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

import com.janrain.backplane.server.config.BackplaneConfig;
import com.janrain.commons.supersimpledb.message.AbstractMessage;
import com.janrain.commons.supersimpledb.message.MessageField;
import com.janrain.crypto.ChannelUtil;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.text.ParseException;
import java.util.*;

/**
 * Token data
 * @author Tom Raney
 */
public class Access extends AbstractMessage {

    private static final int CHANNEL_NAME_LENGTH = 32;
    public static enum type {REGULAR_TOKEN, PRIVILEGED_TOKEN, CODE};
    private static final Logger logger = Logger.getLogger(Access.class);

    /**
     * Empty default constructor for AWS to use.
     * Don't call directly.
     */
    public Access() {};

    /**
     * Create a Access object for storage in SimpleDB
     * @param id
     * @param type "regular_token" or "privileged_token" or "code"
     * @param buses is a space delimited list of buses
     * @param expires is null if the id does not expire
     */
    public Access(@NotNull String id, @NotNull type type, @NotNull String buses,  @Nullable Date expires, boolean createChannel) {
        Map<String,String> d = new LinkedHashMap<String, String>();
        assert(type == type.REGULAR_TOKEN || type == type.PRIVILEGED_TOKEN || type == type.CODE);
        if (type.equals("regular_token") || type.equals("code")) {
            assert( expires != null);
        }
        d.put(Field.ID.getFieldName(), id);
        d.put(Field.TYPE.getFieldName(), type.name());
        if (expires != null) {
            d.put(Field.EXPIRES.getFieldName(), BackplaneConfig.ISO8601.format(expires));
        } else {
            d.put(Field.EXPIRES.getFieldName(), "");
        }

        if (StringUtils.isNotEmpty(buses)) {
            d.put(Field.BUSES.getFieldName(), buses);
        }

        if (createChannel) {
            d.put(Field.CHANNEL.getFieldName(), ChannelUtil.randomString(CHANNEL_NAME_LENGTH));
        }

        super.init(id, d);

    }

    public String getChannelName() {
        return this.get(Field.CHANNEL);
    }

    public String getBusesAsString() {
        return this.get(Field.BUSES);
    }

    public List<String> getBusesAsList() {
        return Arrays.asList(getBusesAsString().split(" "));
    }

    public boolean isAllowedBus(@NotNull String testBus) {
        return getBusesAsList().contains(testBus);
    }

    public boolean isAllowedBuses(@NotNull List<String> testBuses) {
        return getBusesAsList().containsAll(testBuses);
    }

    public Date getExpiresDate() {

        Date expires = null;
        try {
            expires = BackplaneConfig.ISO8601.parse(this.get(Field.EXPIRES));
        } catch (ParseException e) {
            return null;
        }
        return expires;
    }

    public boolean isExpired() {
        //check the code for expiration
        Date expires = getExpiresDate();
        if (expires != null && new Date().getTime() > expires.getTime()) {
            return true;
        }
        return false;
    }

    @Override
    public String getIdValue() {
        return get(Field.ID);
    }

    @Override
    public Set<? extends MessageField> getFields() {
        return EnumSet.allOf(Field.class);
    }

    public static enum Field implements MessageField {
        ID("id"),
        EXPIRES("expires"),
        TYPE("type"),
        BUSES("buses", false),
        CHANNEL("channel", false);

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
}
