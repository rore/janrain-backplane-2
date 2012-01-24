package com.janrain.backplane.server.config;

import com.janrain.commons.supersimpledb.message.MessageField;

import java.util.LinkedHashMap;
import java.util.Map;

/**
 * @author Tom Raney
 */
public class Client extends User {

    /**
     * Empty default constructor for AWS to use
     */
    public Client() {};

    public Client(String client_id, String client_secret, String source_url, String redirect_uri) {
        Map<String,String> d = new LinkedHashMap<String, String>();
        d.put(Field.USER.getFieldName(), client_id);
        d.put(Field.PWDHASH.getFieldName(), client_secret);
        d.put(ClientField.SOURCE_URL.getFieldName(), source_url);
        d.put(ClientField.REDIRECT_URI.getFieldName(), redirect_uri);
        super.init(client_id, d);
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

        SOURCE_URL,
        
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
