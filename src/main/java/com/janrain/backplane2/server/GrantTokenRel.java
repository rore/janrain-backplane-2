/*
 * Copyright 2011 Janrain, Inc.
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

import com.janrain.commons.supersimpledb.message.AbstractMessage;
import com.janrain.commons.supersimpledb.message.MessageField;
import org.apache.commons.lang.StringUtils;

import java.util.*;

/**
 * @author Tom Raney
 */
public class GrantTokenRel extends AbstractMessage {

    /**
     * Empty default constructor for AWS to use.
     * Don't call directly.
     */
    public GrantTokenRel() {};

    public GrantTokenRel(String authId, String tokenId) {
        Map<String,String> d = new LinkedHashMap<String, String>();

        assert(StringUtils.isNotEmpty(authId));
        assert(StringUtils.isNotEmpty(tokenId));
        UUID id = UUID.randomUUID();
        d.put(Field.ID.getFieldName(), id.toString());
        d.put(Field.AUTHID.getFieldName(), authId);
        d.put(Field.TOKENID.getFieldName(), tokenId);

        super.init(id.toString(), d);

    }

    public String getAuthId() {
        return get(Field.AUTHID);
    }

    public String getTokenId() {
        return get(Field.TOKENID);
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
        AUTHID("auth_id"),
        TOKENID("token_id");

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
