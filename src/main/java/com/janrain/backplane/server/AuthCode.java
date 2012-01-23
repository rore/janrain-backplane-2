package com.janrain.backplane.server;

import com.janrain.backplane.server.config.BackplaneConfig;
import com.janrain.commons.supersimpledb.message.MessageField;

import java.text.ParseException;
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

    public AuthCode(String grantId, Date expires) {
        // the AuthCode record shares the same key as the source Grant record
        super(grantId, expires);
    }

    public AuthCode(String grantId) {
        this(grantId, new Date(new Date().getTime() + EXPIRES_SECONDS * 1000L));
    }

    public void setGrant(Grant grant) {
        this.backingGrant = grant;
    }

    public Grant getGrant() {
        return this.backingGrant;
    }

    public String getGrantId() {
        return get(Field.ID);
    }

    public void setUsedNow() {
        put(CodeField.DATE_USED.getFieldName(), BackplaneConfig.ISO8601.format(new Date()));
    }

    public boolean isValid() {
        return getDateUsed() == null;
    }

    public Date getDateUsed() {

        String usedString = get(CodeField.DATE_USED);
        if (usedString == null) {
            return null;
        }

        Date expires = null;
        try {
            expires = BackplaneConfig.ISO8601.parse(usedString);
        } catch (ParseException e) {
            return null;
        }
        return expires;
    }

    // PRIVATE

    private static enum CodeField implements MessageField {
        DATE_USED("dateused");

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

        private CodeField(String fieldName) {
            this(fieldName, true);
        }

        private CodeField(String fieldName, boolean required) {
            this.fieldName = fieldName;
            this.required = required;
        }
    }


    private Grant backingGrant;



}
