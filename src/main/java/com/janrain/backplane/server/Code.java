package com.janrain.backplane.server;

import com.janrain.crypto.ChannelUtil;

import java.util.Date;

/**
 * @author Tom Raney
 */
public class Code extends Access {

    public static final int CODE_LENGTH = 20;
    public static final int EXPIRES_SECONDS = 600;

    public Code(String code, Date expires) {
        super(code, type.CODE, expires, false);
    }

    public Code() {
       this(ChannelUtil.randomString(CODE_LENGTH), new Date(new Date().getTime() + EXPIRES_SECONDS * 1000));
    }

}
