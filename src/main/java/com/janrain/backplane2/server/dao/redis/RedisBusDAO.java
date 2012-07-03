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

package com.janrain.backplane2.server.dao.redis;

import com.janrain.backplane.server.User;
import com.janrain.backplane2.server.BackplaneServerException;
import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.backplane2.server.config.BusConfig2;
import com.janrain.backplane2.server.dao.BusDAO;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import com.janrain.commons.supersimpledb.message.MessageField;
import com.janrain.oauth2.TokenException;
import com.janrain.redis.Redis;
import org.apache.commons.lang.NotImplementedException;
import org.apache.commons.lang.SerializationUtils;
import redis.clients.jedis.Jedis;

import java.util.ArrayList;
import java.util.List;

import static com.janrain.backplane2.server.config.Backplane2Config.SimpleDBTables.BP_BUS_CONFIG;

/**
 * @author Johnny Bufu
 */
public class RedisBusDAO implements BusDAO {

    public static byte[] getKey(String id) {
        return new String("v2_bus_" + id).getBytes();
    }

    @Override
    public List<BusConfig2> retrieveByOwner(String busOwner) throws BackplaneServerException {
        List<BusConfig2> buses = getAll();
        List<BusConfig2> filtered = new ArrayList<BusConfig2>();
        for (BusConfig2 busConfig : buses) {
            if (busOwner.equals(busConfig.get(BusConfig2.Field.OWNER))) {
                filtered.add(busConfig);
            }
        }
        return filtered;
    }

    @Override
    public void deleteByOwner(String busOwner) throws BackplaneServerException, TokenException {
        List<BusConfig2> buses = getAll();
        for (BusConfig2 busConfig: buses) {
            if (busOwner.equals(busConfig.get(BusConfig2.Field.OWNER))) {
                delete(busConfig.getIdValue());
            }
        }
    }

    @Override
    public BusConfig2 get(String id) throws BackplaneServerException {
        byte[] bytes = Redis.getInstance().get(getKey(id));
        if (bytes != null) {
            return (BusConfig2) SerializationUtils.deserialize(bytes);
        } else {
            return null;
        }
    }

    @Override
    public List<BusConfig2> getAll() throws BackplaneServerException {
        List<BusConfig2> buses = new ArrayList<BusConfig2>();
        List<byte[]> bytesList = Redis.getInstance().lrange(getKey("list"), 0, -1);
        for (byte[] bytes : bytesList) {
            if (bytes != null) {
                buses.add((BusConfig2) SerializationUtils.deserialize(bytes));
            }
        }
        return buses;
    }

    @Override
    public void persist(BusConfig2 obj) throws BackplaneServerException {
        byte[] bytes = SerializationUtils.serialize(obj);
        Redis.getInstance().rpush(getKey("list"), bytes);
        Redis.getInstance().set(getKey(obj.getIdValue()), bytes);
    }

    @Override
    public void delete(String id) throws BackplaneServerException, TokenException {
        Jedis jedis = null;
        try {
            jedis = Redis.getInstance().getJedis();
            byte[] bytes = jedis.get(getKey(id));
            if (bytes != null) {
                jedis.lrem(getKey("list"), 0, bytes);
                jedis.del(getKey(id));
            }
        } finally {
            Redis.getInstance().releaseToPool(jedis);
        }
    }

}
