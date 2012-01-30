package com.janrain.backplane2.server;

import com.janrain.commons.supersimpledb.message.AbstractMessage;
import com.janrain.commons.supersimpledb.message.MessageField;

import java.util.EnumSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

/**
 * @author Johnny Bufu
 */
public class AuthSession extends AbstractMessage {

    // - PUBLIC

    /**
     * Empty default constructor for AWS to use
     */
    public AuthSession() { }
    
    public AuthSession(String authUser, String cookie) {
        Map<String,String> data = new LinkedHashMap<String, String>();
        data.put(Field.AUTH_USER.getFieldName(), authUser);
        data.put(Field.COOKIE.getFieldName(), cookie);
        super.init(cookie, data);
    }

    @Override
    public String getIdValue() {
        return get(Field.COOKIE);
    }

    @Override
    public Set<? extends MessageField> getFields() {
        return EnumSet.allOf(Field.class);
    }

    public static enum Field implements MessageField {

        // - PUBLIC

        COOKIE,
        AUTH_USER;

        @Override
        public String getFieldName() {
            return name();
        }

        @Override
        public boolean isRequired() {
            return true;
        }

        @Override
        public void validate(String value) throws RuntimeException {
            if (isRequired()) validateNotNull(name(), value);
        }
    }
}
