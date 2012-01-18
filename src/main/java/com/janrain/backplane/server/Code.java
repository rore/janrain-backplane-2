package com.janrain.backplane.server;

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

    Code(String code, String buses, Date expires) {
        super(code, type.CODE, buses, null, expires, false);
        assert(StringUtils.isNotBlank(buses));
        logger.info("Code declared with buses: " + buses);

    }

    public Code(String buses) {
       this(ChannelUtil.randomString(CODE_LENGTH), buses, new Date(new Date().getTime() + EXPIRES_SECONDS * 1000));
    }

    public Code(String buses, Date expires) {
        this(ChannelUtil.randomString(CODE_LENGTH), buses, expires);
    }

    private static final Logger logger = Logger.getLogger(Access.class);

}
