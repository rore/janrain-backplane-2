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
import com.janrain.backplane.server.User;

/**
 * @author Tom Raney
 */

public class DaoFactory {

    public static BackplaneMessageDAO getBackplaneMessageDAO() {
        return messageDao;
    }

    public static UserDAO getUserDAO() {
        return userDao;
    }

    public static AdminDAO getAdminDAO() {
        return adminDao;
    }

    public static BusConfig1DAO getBusDAO() {
        return busDao;
    }

    public static ConfigDAO getConfigDAO() {
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

    private static BackplaneMessageDAO messageDao = new BackplaneMessageDAO();
    private static UserDAO userDao = new UserDAO();
    private static AdminDAO adminDao = new AdminDAO();
    private static BusConfig1DAO busDao = new BusConfig1DAO();
    private static ConfigDAO configDao = new ConfigDAO();
}
