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

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.jetbrains.annotations.Nullable;
import redis.clients.jedis.Jedis;
import redis.clients.jedis.JedisPool;
import redis.clients.jedis.JedisPoolConfig;
import redis.clients.jedis.Transaction;

import java.util.List;
import java.util.Set;

/**
 * @author Tom Raney
 */
public class Redis {

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
        Jedis jedis = pool.getResource();
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
            if (jedis != null) pool.returnResource(jedis);
        }
        logger.warn("couldn't get lock '" + lockName + " with id " + identifier);
        return null;
    }

    /**
     * Be sure to return to pool!
     * @return
     */

    public Jedis getJedis() {
        return pool.getResource();
    }

    public void releaseToPool(Jedis jedis) {
        if (jedis != null) {
            pool.returnResource(jedis);
        }
    }

    public void releaseBrokenResourceToPool(Jedis jedis) {
        if (jedis != null) {
            pool.returnBrokenResource(jedis);
        }
    }

    public boolean releaseLock(String lockName, String identifier) {
        Jedis jedis = pool.getResource();

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
            pool.returnResource(jedis);
        }
    }

    public boolean refreshLock(String lockName, String identifier, int lockTimeSeconds) {
        Jedis jedis = pool.getResource();

        try {
            // refresh lock
            byte[] currentIdentifier = jedis.get(lockName.getBytes());

            if (currentIdentifier != null && new String(currentIdentifier).equals(identifier)) {
                jedis.expire(lockName, lockTimeSeconds);
                return true;
            }
        } finally {
            pool.returnResource(jedis);
        }
        logger.warn("lock " + lockName + " with identifier " + identifier + " is no longer current");
        return false;
    }

    public void set(byte[] key, byte[] value) {
        Jedis jedis = pool.getResource();

        try {
            jedis.set(key,value);
        } finally {
            pool.returnResource(jedis);
        }
    }

    public void del(byte[] key) {
        Jedis jedis = pool.getResource();
        try {
            jedis.del(key);
        } finally {
            pool.returnResource(jedis);
        }
    }

    public void set(byte[] key, byte[] value, int seconds) {
        Jedis jedis = pool.getResource();
        try {
            jedis.setex(key, seconds, value);
        } finally {
            pool.returnResource(jedis);
        }
    }

    public void set(String key, String value) {
        set(key, value, null);
    }

    public void set(String key, String value, @Nullable Integer seconds) {
        Jedis jedis = pool.getResource();
        try {
            if (seconds == null) {
                jedis.set(key, value);
            } else {
                jedis.setex(key, seconds, value);
            }
        } finally {
            pool.returnResource(jedis);
        }
    }

    public void append(byte[] key, byte[] value) {
        Jedis jedis = pool.getResource();
        try {
            jedis.append(key, value);
        } finally {
            pool.returnResource(jedis);
        }
    }

    public Long rpush(final byte[] key, final byte[] string) {
        Jedis jedis = pool.getResource();
        try {
            return jedis.rpush(key, string);
        } finally {
            pool.returnResource(jedis);
        }
    }

    public long llen(byte[] key) {
        Jedis jedis = pool.getResource();
        try {
            return jedis.llen(key);
        } finally {
            pool.returnResource(jedis);
        }
    }

    public byte[] get(byte[] key) {
        Jedis jedis = pool.getResource();
        try {
            return jedis.get(key);
        } finally {
            pool.returnResource(jedis);
        }
    }

    public String get(String key) {
        Jedis jedis = pool.getResource();
        try {
            return jedis.get(key);
        } finally {
            pool.returnResource(jedis);
        }
    }

    public List<byte[]> mget(byte[]... keys) {
        Jedis jedis = pool.getResource();
        try {
            return jedis.mget(keys);
        } finally {
            pool.returnResource(jedis);
        }
    }

    public byte[] lpop(byte[] key) {
        Jedis jedis = pool.getResource();
        try {
            return jedis.lpop(key);
        } finally {
            pool.returnResource(jedis);
        }
    }

    public List<byte[]> lrange(final byte[] key, final int start, final int end) {
        Jedis jedis = pool.getResource();
        try {
            return jedis.lrange(key, start, end);
        } finally {
            pool.returnResource(jedis);
        }
    }

    public Set<byte[]> zrangebyscore(final byte[] key, double min, double max) {
        Jedis jedis = pool.getResource();
        try {
            return jedis.zrangeByScore(key, min, max);
        } finally {
            pool.returnResource(jedis);
        }
    }

    public long zcard(final byte[] key) {
        Jedis jedis = pool.getResource();
        try {
            return jedis.zcard(key);
        } finally {
            pool.returnResource(jedis);
        }
    }

    // PRIVATE

    private static final Logger logger = Logger.getLogger(Redis.class);

    private final JedisPool pool;

    private static Redis instance = new Redis();

    private Redis() {
        JedisPoolConfig config = new JedisPoolConfig();
        config.setMaxActive(100);
        config.setMaxIdle(100);
        config.setMinIdle(100);

        String redisServerConfig = System.getProperty("REDIS_SERVER_PRIMARY");
        if (StringUtils.isEmpty(redisServerConfig)) {
            logger.error("Cannot find configuration entry for Redis server");
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

        pool = new JedisPool(config, args[0], port);
    }
}
