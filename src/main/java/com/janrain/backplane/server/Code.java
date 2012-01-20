package com.janrain.backplane.server;

import com.janrain.commons.supersimpledb.message.MessageField;
import com.janrain.crypto.ChannelUtil;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;

import java.util.Date;

/**
 * @author Tom Raney
 */
public class Code extends Access {

    public static final int CODE_LENGTH = 20;
    public static final int EXPIRES_SECONDS = 600;

    /**
     * Empty default constructor for AWS to use.
     * Don't call directly.
     */
    public Code() {};

    Code(String clientId, String code, String buses, Date expires) {
        super(code, type.CODE, buses, null, expires, false);
        assert(StringUtils.isNotBlank(buses));
        put(CodeField.ISSUED_TO_CLIENT_ID.getFieldName(), clientId);
        logger.info("Code declared with buses: " + buses);
    }

    public Code(String clientId, String buses) {
        this(clientId, ChannelUtil.randomString(CODE_LENGTH), buses, new Date(new Date().getTime() + EXPIRES_SECONDS * 1000));
    }

    public Code(String clientId, String buses, Date expires) {
        this(clientId, ChannelUtil.randomString(CODE_LENGTH), buses, expires);
    }

    public String getCodeClientId() {
        return get(CodeField.ISSUED_TO_CLIENT_ID.getFieldName());
    }

    private static final Logger logger = Logger.getLogger(Access.class);

    public static enum CodeField implements MessageField {

        // - PUBLIC

        ISSUED_TO_CLIENT_ID;

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
