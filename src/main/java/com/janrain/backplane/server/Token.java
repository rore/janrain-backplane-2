package com.janrain.backplane.server;

import com.janrain.crypto.ChannelUtil;

import java.util.Date;

/**
 * @author Tom Raney
 */

public class Token extends Access {

    public static final int TOKEN_LENGTH = 20;
    public static final int EXPIRES_SECONDS = 3600;
    public static final String ANONYMOUS = "anonymous";

    /**
     * Empty default constructor for AWS to use.
     * Don't call directly.
     */
    public Token() {};

    Token(String token, type accessType, String buses, Date expires) {
        super(token,accessType,buses,expires,accessType == accessType.REGULAR_TOKEN ? true: false);
    }

    public Token(type accessType, String buses, Date expires) {
        this(ChannelUtil.randomString(TOKEN_LENGTH), accessType, buses, expires);
    }

    public Token(Code code) {
        this(ChannelUtil.randomString(TOKEN_LENGTH), type.PRIVILEGED_TOKEN, code.getBusesAsString(), null);
    }

    public String getTokenType() {
        return "Bearer";
    }

    public type getAccessType() {
        return type.valueOf(this.get(Field.TYPE));
    }

    public boolean isPrivileged() {
        return getAccessType() == type.PRIVILEGED_TOKEN;
    }

}
