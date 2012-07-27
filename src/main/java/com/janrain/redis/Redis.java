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

package com.janrain.redis;

import com.janrain.backplane.server.utils.BackplaneSystemProps;
import com.netflix.curator.framework.CuratorFramework;
import com.netflix.curator.framework.recipes.cache.ChildData;
import com.netflix.curator.framework.recipes.cache.PathChildrenCache;
import com.netflix.curator.framework.recipes.cache.PathChildrenCacheEvent;
import com.netflix.curator.framework.recipes.cache.PathChildrenCacheListener;
import com.netflix.curator.framework.recipes.locks.InterProcessMutex;
import org.apache.commons.lang.NotImplementedException;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.jetbrains.annotations.Nullable;
import redis.clients.jedis.Jedis;
import redis.clients.jedis.JedisPool;
import redis.clients.jedis.JedisPoolConfig;
import redis.clients.jedis.Transaction;

import java.util.List;
import java.util.Set;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

/**
 * @author Tom Raney
 */
public class Redis implements PathChildrenCacheListener {

    public static Redis getInstance() {
        return instance;
    }

    /**
     *
     * @param lockName
     * @param identifier
     * @param waitInMilliSeconds if -1 loop forever
     * @param lockTimeSeconds
     * @return
     */

    public String getLock(String lockName, String identifier, int waitInMilliSeconds, int lockTimeSeconds) {
        Jedis jedis = getActivePool().getResource();
        boolean loop = true;

        try {
            long end = System.currentTimeMillis() + waitInMilliSeconds;

            while (loop) {
                if (jedis.setnx(lockName, identifier) == 1) {
                    jedis.expire(lockName, lockTimeSeconds);
                    return identifier;
                } else if (jedis.ttl(lockName) == -1 ) {
                    jedis.expire(lockName, lockTimeSeconds);
                }
                Thread.sleep((int)(Math.random()*20));
                loop = waitInMilliSeconds < 0 || end > System.currentTimeMillis();
            }
        } catch (InterruptedException e) {
            logger.warn(e);
        } finally {
            if (jedis != null) getActivePool().returnResource(jedis);
        }
        logger.warn("couldn't get lock '" + lockName + " with id " + identifier);
        return null;
    }

    /**
     * Be sure to return to pool!
     * @return
     */

    public Jedis getJedis() {
        return getActivePool().getResource();
    }

    public void releaseToPool(Jedis jedis) {
        if (jedis != null) {
            getActivePool().returnResource(jedis);
        }
    }

    public void releaseBrokenResourceToPool(Jedis jedis) {
        if (jedis != null) {
            getActivePool().returnBrokenResource(jedis);
        }
    }

    public boolean releaseLock(String lockName, String identifier) {
        Jedis jedis = getActivePool().getResource();

        try {
            while (true) {
                jedis.watch(lockName);
                String lockHolder = jedis.get(lockName);
                if (identifier.equals(lockHolder)) {
                    Transaction t = jedis.multi();
                    t.del(lockName);
                    if (t.exec() != null) return true;
                } else {
                    logger.warn(identifier + " lost lock to " + lockHolder);
                    jedis.unwatch();
                    return false;
                }
            }
        } finally {
            getActivePool().returnResource(jedis);
        }
    }

    public boolean refreshLock(String lockName, String identifier, int lockTimeSeconds) {
        Jedis jedis = getActivePool().getResource();

        try {
            // refresh lock
            byte[] currentIdentifier = jedis.get(lockName.getBytes());

            if (currentIdentifier != null && new String(currentIdentifier).equals(identifier)) {
                jedis.expire(lockName, lockTimeSeconds);
                return true;
            }
        } finally {
            getActivePool().returnResource(jedis);
        }
        logger.warn("lock " + lockName + " with identifier " + identifier + " is no longer current");
        return false;
    }

    public void set(byte[] key, byte[] value) {
        Jedis jedis = getActivePool().getResource();

        try {
            jedis.set(key,value);
        } finally {
            getActivePool().returnResource(jedis);
        }
    }

    public void del(byte[] key) {
        Jedis jedis = getActivePool().getResource();
        try {
            jedis.del(key);
        } finally {
            getActivePool().returnResource(jedis);
        }
    }

    public void set(byte[] key, byte[] value, int seconds) {
        Jedis jedis = getActivePool().getResource();
        try {
            jedis.setex(key, seconds, value);
        } finally {
            getActivePool().returnResource(jedis);
        }
    }

    public void set(String key, String value) {
        set(key, value, null);
    }

    public void set(String key, String value, @Nullable Integer seconds) {
        Jedis jedis = getActivePool().getResource();
        try {
            if (seconds == null) {
                jedis.set(key, value);
            } else {
                jedis.setex(key, seconds, value);
            }
        } finally {
            getActivePool().returnResource(jedis);
        }
    }

    public void append(byte[] key, byte[] value) {
        Jedis jedis = getActivePool().getResource();
        try {
            jedis.append(key, value);
        } finally {
            getActivePool().returnResource(jedis);
        }
    }

    public Long rpush(final byte[] key, final byte[] string) {
        Jedis jedis = getActivePool().getResource();
        try {
            return jedis.rpush(key, string);
        } finally {
            getActivePool().returnResource(jedis);
        }
    }

    public long llen(byte[] key) {
        Jedis jedis = getActivePool().getResource();
        try {
            return jedis.llen(key);
        } finally {
            getActivePool().returnResource(jedis);
        }
    }

    public byte[] get(byte[] key) {
        Jedis jedis = getActivePool().getResource();
        try {
            return jedis.get(key);
        } finally {
            getActivePool().returnResource(jedis);
        }
    }

    public String get(String key) {
        Jedis jedis = getActivePool().getResource();
        try {
            return jedis.get(key);
        } finally {
            getActivePool().returnResource(jedis);
        }
    }

    public List<byte[]> mget(byte[]... keys) {
        Jedis jedis = getActivePool().getResource();
        try {
            return jedis.mget(keys);
        } finally {
            getActivePool().returnResource(jedis);
        }
    }

    public byte[] lpop(byte[] key) {
        Jedis jedis = getActivePool().getResource();
        try {
            return jedis.lpop(key);
        } finally {
            getActivePool().returnResource(jedis);
        }
    }

    public List<byte[]> lrange(final byte[] key, final int start, final int end) {
        Jedis jedis = getActivePool().getResource();
        try {
            return jedis.lrange(key, start, end);
        } finally {
            getActivePool().returnResource(jedis);
        }
    }

    public Set<byte[]> zrangebyscore(final byte[] key, double min, double max) {
        Jedis jedis = getActivePool().getResource();
        try {
            return jedis.zrangeByScore(key, min, max);
        } finally {
            getActivePool().returnResource(jedis);
        }
    }

    public long zcard(final byte[] key) {
        Jedis jedis = getActivePool().getResource();
        try {
            return jedis.zcard(key);
        } finally {
            getActivePool().returnResource(jedis);
        }
    }

    public void setActiveRedisInstance(CuratorFramework client) {
        this.curatorFramework = client;
        InterProcessMutex lock = null;

        try {
            lock = new InterProcessMutex(client, REDIS_LOCK);
            lock.acquire();

            PathChildrenCache pathChildrenCache = new PathChildrenCache(client, REDIS, true);
            pathChildrenCache.getListenable().addListener(this);
            pathChildrenCache.start(true);

            ChildData childData = pathChildrenCache.getCurrentData(REDIS);

            String redisServer = null;
            if (childData != null) {
                byte[] bytes = childData.getData();
                redisServer = new String(bytes);
            }

            if (redisServer == null) {
                // set the node to the default redis server
                setRedisServer(BackplaneSystemProps.REDIS_SERVER_PRIMARY);
            } else {
                // accept the cluster wide redis server
                currentRedisServer = redisServer;
            }

        } catch (Exception e) {
            logger.error(e);
        } finally {
            if (lock != null) {
                try {
                    lock.release();
                } catch (Exception e) {
                    logger.error("could not release lock " + e);
                }
            }
        }

    }

    @Override
    public void childEvent(CuratorFramework curatorFramework, PathChildrenCacheEvent pathChildrenCacheEvent) throws Exception {
        try {
            if ( pathChildrenCacheEvent.getType() == PathChildrenCacheEvent.Type.CHILD_UPDATED
                    || pathChildrenCacheEvent.getType() == PathChildrenCacheEvent.Type.CHILD_ADDED) {

                ChildData childData = pathChildrenCacheEvent.getData();
                if (childData != null && childData.getData() != null)  {
                    String newValue = new String(childData.getData());
                    currentRedisServer = newValue;
                    logger.info("redis server changed: " + newValue);
                }
            }
        } catch (Exception e) {
            logger.warn("failure with childEvent");
            throw e;
        }

    }

    public synchronized void setRedisServer(String server) {

        if (curatorFramework != null) {
            try {
                if (this.curatorFramework.checkExists().forPath(REDIS_SERVER) == null) {
                    this.curatorFramework.create().forPath(REDIS_SERVER);
                }
                this.curatorFramework.setData().forPath(REDIS_SERVER, server.getBytes());
            } catch (Exception e) {
                logger.error(e);
            }
        }
        logger.info("redis server set to " + server);
    }

    public void ping() {
        Jedis jedis = null;
        try {
            jedis = getActivePool().getResource();
            logger.info("PING " + currentRedisServer);
            logger.info(jedis.ping());
        } catch (Exception e) {
            // something bad
            // if currently set to primary redis server, switch to secondary
            if (BackplaneSystemProps.REDIS_SERVER_PRIMARY.equals(currentRedisServer)) {
                setRedisServer(BackplaneSystemProps.REDIS_SERVER_SECONDARY);
            }
        } finally {
            getActivePool().returnResource(jedis);
        }

    }

    // PRIVATE

    private static final Logger logger = Logger.getLogger(Redis.class);
    private String currentRedisServer;

    private final JedisPool pool1;
    private final JedisPool pool2;

    private static Redis instance = new Redis();
    private final String REDIS_LOCK = "/redislock";
    private final String REDIS = "/redis";
    private final String REDIS_SERVER = "/redis/server";

    private CuratorFramework curatorFramework;

    private Redis() {
        JedisPoolConfig config = new JedisPoolConfig();
        config.setMaxActive(100);
        config.setMaxIdle(100);
        config.setMinIdle(100);

        String redisServerConfig = System.getProperty("REDIS_SERVER_PRIMARY");
        if (StringUtils.isEmpty(redisServerConfig)) {
            logger.error("Cannot find configuration entry for primary Redis server");
            System.exit(1);
        }
        String[] args = redisServerConfig.split(":");
        int port = 6379;
        if (args.length == 2) {
            try {
                port = Integer.parseInt(args[1]);
            } catch (NumberFormatException e) {
                logger.error("port for Redis server is malformed: " + redisServerConfig);
            }
        }

        pool1 = new JedisPool(config, args[0], port);

        redisServerConfig = System.getProperty("REDIS_SERVER_SECONDARY");
        if (StringUtils.isEmpty(redisServerConfig)) {
            logger.error("Cannot find configuration entry for secondary Redis server");
            System.exit(1);
        }
        args = redisServerConfig.split(":");
        port = 6379;
        if (args.length == 2) {
            try {
                port = Integer.parseInt(args[1]);
            } catch (NumberFormatException e) {
                logger.error("port for Redis server is malformed: " + redisServerConfig);
            }
        }

        pool2 = new JedisPool(config, args[0], port);

    }

    
    private JedisPool getActivePool() {
        if (BackplaneSystemProps.REDIS_SERVER_PRIMARY.equals(currentRedisServer)) {
            return pool1;
        } else if (BackplaneSystemProps.REDIS_SERVER_SECONDARY.equals(currentRedisServer)) {
            return pool2;
        } else {
            return null;
        }
    }

}
