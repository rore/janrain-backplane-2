package com.janrain.backplane.server;

import com.janrain.commons.supersimpledb.message.MessageField;
import com.janrain.crypto.ChannelUtil;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.Date;
import java.util.List;

/**
 * @author Tom Raney
 */
public class Code extends Base {

    public static final int CODE_LENGTH = 20;
    // ten minutes per recommendation @ http://tools.ietf.org/html/draft-ietf-oauth-v2-22#section-4.1.2
    public static final int EXPIRES_SECONDS = 600;

    /**
     * Empty default constructor for AWS to use.
     * Don't call directly.
     */
    public Code() {};

    Code(String code, String clientId, String buses, Date expires) {
        super(code, buses, expires);

        assert(StringUtils.isNotEmpty(clientId));

        put(CodeField.ISSUED_TO_CLIENT_ID.getFieldName(), clientId);
        logger.info("Code declared with buses: " + buses);
    }

    public Code(String clientId, String buses) {
        this(ChannelUtil.randomString(CODE_LENGTH), clientId, buses, new Date(new Date().getTime() + EXPIRES_SECONDS * 1000));
    }

    public Code(String clientId, String buses, Date expires) {
        this(ChannelUtil.randomString(CODE_LENGTH), clientId, buses, expires);
    }

    public String getCodeClientId() {
        return get(CodeField.ISSUED_TO_CLIENT_ID.getFieldName());
    }


    public static enum CodeField implements MessageField {

        // - PUBLIC

        ISSUED_BY_USER_ID("issued_by_user", false),
        ISSUED_TO_CLIENT_ID("issued_to_client", false);

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

    private static final Logger logger = Logger.getLogger(Access.class);

}
