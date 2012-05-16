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

import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.message.MessageField;
import org.apache.commons.lang.StringUtils;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;

/**
 * @author Tom Raney
 */
public abstract class Base extends Access {

    /**
     * Empty default constructor for AWS to use.
     * Don't call directly.
     */
    public Base() {}

    public Base(String id, @Nullable String buses, Date expires) throws SimpleDBException {
        super(id, expires);

        if (StringUtils.isNotEmpty(buses)) {
            put(BaseField.BUSES.getFieldName(), buses);
        }

    }

    public @Nullable
    String getBusesAsString() {
        return this.get(BaseField.BUSES);
    }

    /**
     * Retrieve list of authorized buses
     * @return a valid list which may be empty
     */
    public @NotNull
    List<String> getBusesAsList() {
        String busesAsString = getBusesAsString();
        if (StringUtils.isEmpty(busesAsString)) {
            return new ArrayList<String>();
        } else {
            return Arrays.asList(busesAsString.split(" "));
        }
    }

    public boolean isAllowedBus(@NotNull String testBus) {
        return getBusesAsList().contains(testBus);
    }

    public boolean isAllowedBuses(@NotNull List<String> testBuses) {
        return getBusesAsList().containsAll(testBuses);
    }

    /**
     * Retrieve an encoded space delimited string of authorized buses
     * as "bus:thisbus.com bus:andthatbus.com ..."
     * @return
     */

    public String getEncodedBusesAsString() {
        StringBuilder sb = new StringBuilder();
        for (String bus: getBusesAsList()) {
            sb.append("bus:" + bus + " ");
        }
        return sb.toString().trim();
    }

    @Override
    public Set<? extends MessageField> getFields() {
        return EnumSet.allOf(BaseField.class);
    }

    public static enum BaseField implements MessageField {

        // - PUBLIC

        BUSES("buses", false);

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
            if (isRequired()) validateNotNull(getFieldName(), value);
        }

        // - PRIVATE

        private String fieldName;
        private boolean required = true;

        private BaseField(String fieldName) {
            this(fieldName, true);
        }

        private BaseField(String fieldName, boolean required) {
            this.fieldName = fieldName;
            this.required = required;
        }
    }
}
