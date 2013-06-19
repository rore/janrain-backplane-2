package com.janrain.backplane.server.redisdao;

/**
 * @author Johnny Bufu
 */
public class BP1DAOs {

    public static BP1MessageDao getMessageDao() {
        return messageDao;
    }

    // - PRIVATE

    private BP1DAOs() {}

    private static final BP1MessageDao messageDao = new RedisBackplaneMessageDAO();

}
