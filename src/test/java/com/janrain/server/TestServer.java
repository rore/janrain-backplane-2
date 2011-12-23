package com.janrain.server;


import com.janrain.backplane.server.BackplaneController;
import com.janrain.crypto.ChannelUtil;
import org.apache.catalina.util.Base64;
import org.apache.log4j.Logger;
import org.junit.Test;

import static org.junit.Assert.assertTrue;

public class TestServer {

    private static final Logger logger = Logger.getLogger(BackplaneController.class);

    @Test
    public void testChannelGeneration() {
        String channel = ChannelUtil.randomString(1000);
        logger.debug(channel);
        assertTrue(Base64.isBase64(channel));
    }


}
