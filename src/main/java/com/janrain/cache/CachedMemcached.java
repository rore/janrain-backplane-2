package com.janrain.cache;

import net.spy.memcached.MemcachedClient;
import net.spy.memcached.OperationTimeoutException;
import net.spy.memcached.internal.OperationFuture;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;

import java.net.InetSocketAddress;
import java.net.SocketAddress;
import java.util.Collection;
import java.util.Date;

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

    @Override
    public synchronized Object getObject(String key) {
        if (client == null || !isEnabled) {
            return null;
        }
        key = makeValidKey(key);
        try {
            return client.get(key);
        } catch (Exception e) {
            logger.warn("'get' call to memcached server failed.  disabling cache for " + RETRY_IN/1000 + " seconds", e);
            // disable
            isEnabled = false;
            new RetryThread().start();
            return null;
        }
    }

    @Override
    public synchronized void setObject(String key, int expiration, Object obj) {
        if (client == null || !isEnabled) {
            return;
        }
        key = makeValidKey(key);
        OperationFuture future = client.set(key, expiration, obj);
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
                        if (client != null) {
                            client.get("foo");
                        }
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
    private MemcachedClient client;
    private boolean isEnabled = false;
    private final long RETRY_IN = 60000l;
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
            InetSocketAddress sock = new InetSocketAddress(memcachedServer, 11211);
            client = new MemcachedClient(sock);
            // test ping
            getObject("foo");
            Collection<SocketAddress> servers = client.getAvailableServers();
            if (servers.isEmpty()) {
                logger.warn("could not reach memcached server(s)");
                isEnabled = false;
                new RetryThread().start();
                return;
            } else {
                for (SocketAddress server : servers) {
                    logger.info("connected to memcached server: " + server.toString());
                }
            }

            isEnabled = true;
            logger.info("memcached enabled");
        } catch (Exception e) {
            logger.error(e);
        }
    }


}
