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

package com.janrain.backplane.server.dao;

import com.janrain.backplane.server.BusConfig1;
import com.janrain.backplane.server.dao.redis.*;
import com.janrain.backplane2.server.config.User;
import com.janrain.backplane2.server.dao.redis.RedisAdminDAO;

/**
 * @author Tom Raney
 */

public class DaoFactory {

    public static RedisBackplaneMessageDAO getBackplaneMessageDAO() {
        return messageDao;
    }

    public static RedisUserDAO getUserDAO() {
        return userDao;
    }

    public static RedisAdminDAO getAdminDAO() {
        return adminDao;
    }

    public static RedisBusConfig1DAO getBusDAO() {
        return busDao;
    }

    public static RedisConfigDAO getConfigDAO() {
        return configDao;
    }

    public static DAO getDaoByObjectType(Class<?> obj) {
        if (User.class.isAssignableFrom(obj)) {
            return getUserDAO();
        } else if (BusConfig1.class.isAssignableFrom(obj)) {
            return getBusDAO();
        }

        return null;
    }

    // - PRIVATE

    private DaoFactory() {}

    private static RedisBackplaneMessageDAO messageDao = new RedisBackplaneMessageDAO();
    private static RedisUserDAO userDao = new RedisUserDAO();
    private static RedisAdminDAO adminDao = new RedisAdminDAO();
    private static RedisBusConfig1DAO busDao = new RedisBusConfig1DAO();
    private static RedisConfigDAO configDao = new RedisConfigDAO();
}
