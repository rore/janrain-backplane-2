package com.janrain.backplane.server;

import com.janrain.crypto.ChannelUtil;

import java.util.Date;

/**
 * @author Tom Raney
 */

public class Token extends Access {

    public static final int TOKEN_LENGTH = 20;
    public static final int EXPIRES_SECONDS = 3600;

    public Token(String token, type type, Date expires) {
        super(token,type,expires,  type == type.REGULAR_TOKEN ? true: false);
    }

    public Token(type type) {
        this(ChannelUtil.randomString(TOKEN_LENGTH), type, null);
    }

    public Token(type type, Date expires) {
        this(ChannelUtil.randomString(TOKEN_LENGTH), type, expires);
    }

    public String getTokenType() {
        return "Bearer";
    }




}
