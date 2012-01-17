package com.janrain.backplane.server.config;

import com.janrain.commons.supersimpledb.message.AbstractMessage;
import com.janrain.commons.supersimpledb.message.MessageField;

import java.io.FileDescriptor;
import java.util.EnumSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

/**
 * @author Tom Raney
 */
public class Client extends User {

    /**
     * Empty default constructor for AWS to use
     */
    public Client() {};

    public Client(String id, String client_secret, String redirect_uri) {
        Map<String,String> d = new LinkedHashMap<String, String>();
        d.put(Field.USER.getFieldName(), id);
        d.put(Field.PWDHASH.getFieldName(), client_secret);
        d.put(ClientField.REDIRECT_URI.getFieldName(), redirect_uri);
        super.init(id, d);
    }

    public String getClientId() {
        return this.get(Field.USER);
    }

    public String getClientSecret() {
        return this.get(Field.PWDHASH);
    }

    public String getRedirectUri() {
        return this.get(ClientField.REDIRECT_URI);
    }

    public static enum ClientField implements MessageField {

        // - PUBLIC

        REDIRECT_URI;

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
