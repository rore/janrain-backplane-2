package com.janrain.backplane.server;

import com.janrain.commons.supersimpledb.message.MessageField;

import java.util.Date;

/**
 * @author Tom Raney
 */
public class AuthCode extends Access {

    /**
     * Empty default constructor for AWS to use
     */
    public AuthCode() {};

    public AuthCode(String authZId, Date expires) {
        super();
        put(AuthCodeField.SOURCE_AUTHZ.getFieldName(), authZId);
    }

    public static enum AuthCodeField implements MessageField {

        // - PUBLIC

        SOURCE_AUTHZ;

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
