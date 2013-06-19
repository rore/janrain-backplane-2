package com.janrain.backplane2.server.dao;

import com.janrain.backplane2.server.dao.redis.*;
import org.springframework.context.annotation.Scope;
import org.springframework.stereotype.Service;

/**
 * @author Tom Raney
 */

@Service(value="redisDaoFactory")
@Scope(value="singleton")
public class BP2DAOs {

    public static ChannelDAO getChannelDao() {
        return channelDao;
    }

    public static TokenDAO getTokenDao() {
        return tokenDao;
    }

    public static BackplaneMessageDAO getBackplaneMessageDAO() {
        return messageDao;
    }

    // - PRIVATE

    private static final TokenDAO tokenDao = new RedisTokenDAO();
    private static final ChannelDAO channelDao = new RedisChannelDAO();
    private static final BackplaneMessageDAO messageDao = new RedisBackplaneMessageDAO();

}
