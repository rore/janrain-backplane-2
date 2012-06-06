/*
 * Copyright 2012 Janrain, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.janrain.cache;

import net.spy.memcached.AddrUtil;
import net.spy.memcached.BinaryConnectionFactory;
import net.spy.memcached.MemcachedClient;
import net.spy.memcached.OperationTimeoutException;
import net.spy.memcached.internal.OperationFuture;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;

import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.util.Collection;
import java.util.Date;
import java.util.List;
import java.util.Map;

/**
 * @author Tom Raney
 */
public class CachedMemcached implements Cached {

    public static synchronized CachedMemcached getInstance() {
        if (instance == null) {
            instance = new CachedMemcached();
        }
        return instance;
    }

    public Map<String, Object> getBulk(List<String> keys) {
        if (client == null || !isEnabled) {
            return null;
        }

        try {
            return getCache().getBulk(keys);
        } catch (Exception e) {
            logger.warn("'getBulk' call to memcached server failed.  disabling cache for " + RETRY_IN/1000 + " seconds", e);
            // disable
            isEnabled = false;
            new RetryThread().start();
            return null;

        }
    }

    @Override
    public Object getObject(String key) {
        if (client == null || !isEnabled) {
            return null;
        }

        key = makeValidKey(key);
        try {
            return getCache().get(key);
        } catch (Exception e) {
            logger.warn("'get' call to memcached server failed.  disabling cache for " + RETRY_IN/1000 + " seconds", e);
            // disable
            isEnabled = false;
            new RetryThread().start();
            return null;
        }
    }

    @Override
    public void setObject(String key, int expiration, Object obj) {
        if (client == null || !isEnabled) {
            return;
        }
        key = makeValidKey(key);
        OperationFuture future = getCache().set(key, expiration, obj);
        try {
            Boolean result = (Boolean)future.get();
            if (result == false) {
                logger.warn("'set' call to memcached server failed");
            }
        } catch (Exception e) {
            logger.warn("'set' call to memcached server failed", e);
        }
    }

    @Override
    public boolean isEnabled() {
        return isEnabled;
    }

    // - PRIVATE

    private class RetryThread extends Thread {

        @Override
        public void run() {
            try {
                while (true) {
                    logger.warn("memcached disabled - retrying in " + RETRY_IN/1000 + " seconds");
                    Thread.sleep(RETRY_IN);
                    try {
                        getCache().get("foo");
                        // success, stop testing process
                        logger.warn("memcached re-enabled");
                        isEnabled = true;
                        return;
                    } catch (Exception ex) {
                        // try again
                        logger.warn("memcached still disabled");
                        isEnabled= false;
                    }
                }
            } catch (InterruptedException e) {
                return;
            }
        }
    }

    private static CachedMemcached instance;
    private MemcachedClient[] client;
    private boolean isEnabled = false;
    private final long RETRY_IN = 30000l;
    private static final Logger logger = Logger.getLogger(CachedMemcached.class);

    /**
     * Must remove white spaces from key String to conform with Memcached protocol
     * @param key
     * @return
     */
    private static String makeValidKey(String key) {
        return key.replaceAll("\\s+", "");
    }

    private CachedMemcached() {
        try {
            String memcachedServer = System.getProperty("MEMCACHED");
            if (StringUtils.isEmpty(memcachedServer)) {
                logger.warn("no memcached server found");
                return;
            }

            logger.info("creating memcached client");

            client = new MemcachedClient[20];
            for (int i=0 ; i< 20; i++) {
                MemcachedClient c = new MemcachedClient(new BinaryConnectionFactory(),
                        AddrUtil.getAddresses(memcachedServer + ":11211"));
                client[i] = c;
            }

            isEnabled = true;
            logger.info("memcached enabled");
        } catch (Exception e) {
            logger.error(e);
        }
    }

    private MemcachedClient getCache() {
        return client[(int)(Math.random()*19)];
    }


}
