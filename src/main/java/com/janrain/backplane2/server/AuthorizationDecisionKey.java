package com.janrain.backplane2.server;

import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.commons.supersimpledb.message.AbstractMessage;
import com.janrain.commons.supersimpledb.message.MessageField;
import com.janrain.crypto.ChannelUtil;

import java.util.*;

/**
 * @author Johnny Bufu
 */
public class AuthorizationDecisionKey extends AbstractMessage {

    // - PUBLIC

    /**
     * Empty default constructor for AWS to use
     */
    public AuthorizationDecisionKey() { }

    public AuthorizationDecisionKey(String authCookie) {
        Map<String,String> data = new LinkedHashMap<String, String>();
        String key = ChannelUtil.randomString(AUTHORIZATION_DECISION_KEY_LENGTH);
        data.put(Field.KEY.getFieldName(), key);
        data.put(Field.AUTH_COOKIE.getFieldName(), authCookie);
        data.put(Field.EXPIRES.getFieldName(), Backplane2Config.ISO8601.format(new Date(System.currentTimeMillis() + AUTHORIZATION_DECISION_TIMEOUT_SECONDS * 1000)));
        super.init(key, data);
    }

    @Override
    public String getIdValue() {
        return get(Field.KEY);
    }

    @Override
    public Set<? extends MessageField> getFields() {
        return EnumSet.allOf(Field.class);
    }

    public static enum Field implements MessageField {

        KEY,
        AUTH_COOKIE,
        EXPIRES;

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

    // - PRIVATE

    private static final int AUTHORIZATION_DECISION_KEY_LENGTH = 30;
    private static final long AUTHORIZATION_DECISION_TIMEOUT_SECONDS = 300l;
}
