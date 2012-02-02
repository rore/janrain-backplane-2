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
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.text.ParseException;
import java.util.*;

/**
 * Access data
 * @author Tom Raney
 */
public abstract class Access extends AbstractMessage {

    /**
     * Empty default constructor for child classes to use
     */
    public Access() {};

    /**
     * Create a Access object for storage in SimpleDB
     * @param id
     * @param expires is null if the id does not expire
     */
    public Access(@NotNull String id, @Nullable Date expires) {
        Map<String,String> d = new LinkedHashMap<String, String>();

        assert(StringUtils.isNotEmpty(id));
        d.put(Field.ID.getFieldName(), id);

        if (expires != null) {
            d.put(Field.EXPIRES.getFieldName(), Backplane2Config.ISO8601.format(expires));
        } else {
            d.put(Field.EXPIRES.getFieldName(), "");
        }

        super.init(id, d);

    }

    public Date getExpiresDate() {

        String expiresString = get(Field.EXPIRES);

        if (expiresString == null) {
            return null;
        }

        Date expires = null;
        try {
            expires = Backplane2Config.ISO8601.parse(expiresString);
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
        EXPIRES("expires");

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
