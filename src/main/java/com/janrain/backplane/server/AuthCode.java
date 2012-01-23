package com.janrain.backplane.server;

import com.janrain.commons.supersimpledb.message.MessageField;
import com.janrain.crypto.ChannelUtil;

import java.util.Date;

/**
 * @author Tom Raney
 */
public class AuthCode extends Access {

    public static final int CODE_LENGTH = 20;
    // ten minutes per recommendation @ http://tools.ietf.org/html/draft-ietf-oauth-v2-22#section-4.1.2
    public static final int EXPIRES_SECONDS = 600;



    /**
     * Empty default constructor for AWS to use
     */
    public AuthCode() {};

    public AuthCode(String codeId, String grantId, Date expires) {
        super(codeId, expires);
        put(AuthCodeField.SOURCE_GRANT.getFieldName(), grantId);
    }

    public AuthCode(String grantId, Date expires) {
        this(ChannelUtil.randomString(CODE_LENGTH), grantId, expires);
    }

    public AuthCode(String grantId) {
        this(ChannelUtil.randomString(CODE_LENGTH), grantId, new Date(new Date().getTime() + EXPIRES_SECONDS * 1000L));
    }

    public void setGrant(Grant grant) {
        this.backingGrant = grant;
    }

    public Grant getGrant() {
        return this.backingGrant;
    }

    public String getGrantId() {
        return get(AuthCodeField.SOURCE_GRANT);
    }

    public static enum AuthCodeField implements MessageField {

        // - PUBLIC

        SOURCE_GRANT;

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

    private Grant backingGrant;



}
