package com.janrain.backplane.server;

import com.janrain.commons.supersimpledb.message.MessageField;
import com.janrain.crypto.ChannelUtil;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import sun.reflect.generics.reflectiveObjects.NotImplementedException;

import java.util.Date;
import java.util.List;

/**
 * @author Tom Raney
 */
public class Grant extends Base {

    public static final int CODE_LENGTH = 20;

    public static String getBusesAsString(List<Grant> grants) {
        StringBuilder sb = new StringBuilder();
        for (Grant grant: grants) {
            sb.append(grant.getBusesAsString() + " ");
        }
        return sb.toString().trim();
    }


    /**
     * Empty default constructor for AWS to use.
     * Don't call directly.
     */
    public Grant() {};

    Grant(String code, String clientId, String buses, Date expires) {
        super(code, buses, expires);

        assert(StringUtils.isNotEmpty(clientId));

        put(CodeField.ISSUED_TO_CLIENT_ID.getFieldName(), clientId);
        logger.info("Grant declared with buses: " + buses);
    }

    public Grant(String clientId, String buses) {
        this(ChannelUtil.randomString(CODE_LENGTH), clientId, buses, null);
    }

    public Grant(String clientId, String buses, Date expires) {
        this(ChannelUtil.randomString(CODE_LENGTH), clientId, buses, expires);
    }

    public String getCodeClientId() {
        return get(CodeField.ISSUED_TO_CLIENT_ID.getFieldName());
    }

    public void revokeAuth() {
        throw new NotImplementedException();
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
