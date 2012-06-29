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

package com.janrain.backplane.server.provision;

import com.janrain.backplane.server.BusConfig1;
import com.janrain.backplane.server.User;
import com.janrain.backplane.server.config.AuthException;
import com.janrain.backplane.server.config.Backplane1Config;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import com.janrain.commons.supersimpledb.message.AbstractMessage;
import com.janrain.crypto.HmacHashUtils;
import org.apache.log4j.Logger;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;

import javax.inject.Inject;
import javax.servlet.http.HttpServletResponse;
import java.util.HashMap;
import java.util.LinkedHashMap;
import java.util.List;
import java.util.Map;

import static com.janrain.backplane.server.config.Backplane1Config.SimpleDBTables.*;

/**
 * Controller handling the API calls for backplane customer configuration provisioning.
 *
 * @author Johnny Bufu
 */
@Controller
@RequestMapping(value="/provision/*")
@SuppressWarnings({"UnusedDeclaration"})
public class ProvisioningController {

    // - PUBLIC

    @RequestMapping(value = "/bus/list", method = RequestMethod.POST)
    @ResponseBody
    public Map<String, Map<String, String>> busList(@RequestBody ListRequest listRequest) throws AuthException {
        checkAdminAuth(listRequest.getAdmin(), listRequest.getSecret());
        return doList(bpConfig.getTableName(BP1_BUS_CONFIG), BusConfig1.class, listRequest.getEntities());
    }

    @RequestMapping(value = "/user/list", method = RequestMethod.POST)
    @ResponseBody
    public Map<String, Map<String, String>> userList(@RequestBody ListRequest listRequest) throws AuthException {
        checkAdminAuth(listRequest.getAdmin(), listRequest.getSecret());
        return doList(bpConfig.getTableName(BP1_USERS), User.class, listRequest.getEntities());
    }

    @RequestMapping(value = "/bus/delete", method = RequestMethod.POST)
    @ResponseBody
    public Map<String, String> busDelete(@RequestBody ListRequest deleteRequest) throws AuthException {
        checkAdminAuth(deleteRequest.getAdmin(), deleteRequest.getSecret());
        return doDelete(bpConfig.getTableName(BP1_BUS_CONFIG), BusConfig1.class, deleteRequest.getEntities());
    }

    @RequestMapping(value = "/user/delete", method = RequestMethod.POST)
    @ResponseBody
    public Map<String, String> userDelete(@RequestBody ListRequest deleteRequest) throws AuthException {
        checkAdminAuth(deleteRequest.getAdmin(), deleteRequest.getSecret());
        return doDelete(bpConfig.getTableName(BP1_USERS), User.class, deleteRequest.getEntities());
    }

    @RequestMapping(value = "/bus/update", method = RequestMethod.POST)
    @ResponseBody
    public Map<String, String> busUpdate(@RequestBody BusUpdateRequest updateRequest) throws AuthException, SimpleDBException {
        return doUpdate(bpConfig.getTableName(BP1_BUS_CONFIG), BusConfig1.class, updateRequest);
    }

    @RequestMapping(value = "/user/update", method = RequestMethod.POST)
    @ResponseBody
    public Map<String, String> userUpdate(@RequestBody UserUpdateRequest updateRequest) throws AuthException, SimpleDBException {
        return doUpdate(bpConfig.getTableName(BP1_USERS), User.class, updateRequest);
    }

    /**
     * Handle auth errors
     */
    @ExceptionHandler
    @ResponseBody
    public Map<String, String> handle(final AuthException e, HttpServletResponse response) {
        logger.error("Provisioning authentication error: " + e.getMessage());
        response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
        return new HashMap<String,String>() {{
            put(ERR_MSG_FIELD, e.getMessage());
        }};
    }

    /**
     * Handle auth SimpleDB errors
     */
    @ExceptionHandler
    @ResponseBody
    public Map<String, String> handle(final SimpleDBException e, HttpServletResponse response) {
        logger.error("Provisioning authentication error: " + e.getMessage());
        response.setStatus(HttpServletResponse.SC_INTERNAL_SERVER_ERROR);
        return new HashMap<String,String>() {{
            put(ERR_MSG_FIELD, e.getMessage());
        }};
    }

    /**
     * Handle all other errors
     */
    @ExceptionHandler
    @ResponseBody
    public Map<String, String> handle(final Exception e, HttpServletResponse response) {
        logger.error("Error handling provisioning request", bpConfig.getDebugException(e));
        response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
        return new HashMap<String,String>() {{
            put(ERR_MSG_FIELD, e.getMessage());
        }};
    }

    // - PRIVATE

    private static final Logger logger = Logger.getLogger(ProvisioningController.class);

    private static final String BACKPLANE_UPDATE_SUCCESS = "BACKPLANE_UPDATE_SUCCESS";
    private static final String BACKPLANE_DELETE_SUCCESS = "BACKPLANE_DELETE_SUCCESS";
    private static final String ERR_MSG_FIELD = "ERR_MSG";
    private static final String CONFIG_NOT_FOUND = "CONFIG_NOT_FOUND";

    @Inject
    private Backplane1Config bpConfig;

    @Inject
    private SuperSimpleDB superSimpleDb;

    private void checkAdminAuth(String user, String password) throws AuthException {
        checkAuth("v1_admin", user, password);
    }

    private void checkAuth(String authTable, String user, String password) throws AuthException {
        try {
            User userEntry = superSimpleDb.retrieve(authTable, User.class, user);
            String authKey = userEntry == null ? null : userEntry.get(User.Field.PWDHASH);
            if ( ! HmacHashUtils.checkHmacHash(password, authKey) ) {
                throw new AuthException("User " + user + " not authorized in " + authTable);
            }
        } catch (SimpleDBException e) {
            throw new AuthException("User " + user + " not authorized in " + authTable + " , " + e.getMessage(), e);
        }
    }

    private <T extends AbstractMessage> Map<String, Map<String, String>> doList(String tableName, Class<T> entityType, List<String> entityNames) {

        if (entityNames.size() == 0) return doListAll(tableName, entityType);

        final Map<String,Map<String,String>> result = new LinkedHashMap<String, Map<String, String>>();
        for(String entityName : entityNames) {
            T config = null;
            Exception thrown = null;
            try {
                config = superSimpleDb.retrieve(tableName, entityType, entityName);
            } catch (Exception e) {
                thrown = e;
            }
            final String errMgs = thrown != null ? thrown.getMessage() : config == null ? CONFIG_NOT_FOUND : null;

            result.put(entityName,
                errMgs != null ? new HashMap<String, String>() {{ put(ERR_MSG_FIELD, errMgs); }} :
                config);
        }
        return result;
    }

    private <T extends AbstractMessage> Map<String, Map<String, String>> doListAll(String tableName, Class<T> entityType) {
        Map<String,Map<String,String>> result = new LinkedHashMap<String, Map<String, String>>();
        try {
            for(T config :  superSimpleDb.retrieveAll(tableName, entityType)) {
                result.put(config.getIdValue(), config);
            }
        } catch (final Exception e) {
            result.put(ERR_MSG_FIELD, new HashMap<String, String>() {{ put(ERR_MSG_FIELD, e.getMessage()); }});
        }
        return result;
    }

    private <T extends AbstractMessage> Map<String, String> doDelete(String tableName, Class<T> entityType, List<String> entityNames) {
        Map<String,String> result = new LinkedHashMap<String, String>();
        for(String entityName : entityNames) {
            String deleteStatus = BACKPLANE_DELETE_SUCCESS;
            try {
                superSimpleDb.delete(tableName, entityName);
            } catch (Exception e) {
                deleteStatus = e.getMessage();
            }
            result.put(entityName, deleteStatus);
        }
        return result;
    }

    private <T extends AbstractMessage> Map<String, String> doUpdate(String tableName, Class<T> entityType, UpdateRequest<T> updateRequest) throws AuthException, SimpleDBException {
        checkAdminAuth(updateRequest.getAdmin(), updateRequest.getSecret());
        validateConfigs(entityType, updateRequest);
        return updateConfigs(tableName, entityType, updateRequest.getConfigs());
    }

    private <T extends AbstractMessage> void validateConfigs(Class<T> entityType, UpdateRequest<T> updateRequest) throws SimpleDBException {
        for(T config : updateRequest.getConfigs()) {
            config.validate();
        }
    }

    private <T extends AbstractMessage> Map<String, String> updateConfigs(String tableName, Class<T> customerConfigType, List<T> bpConfigs) {
        Map<String,String> result = new LinkedHashMap<String, String>();
        for(T config : bpConfigs) {
            if (config instanceof User) {
                // hash the new user password
                User user = (User) config;
                user.put(User.Field.PWDHASH.getFieldName(), HmacHashUtils.hmacHash(user.get(User.Field.PWDHASH)));
            }
            String updateStatus = BACKPLANE_UPDATE_SUCCESS;
            try {
                superSimpleDb.store(tableName, customerConfigType, config);
            } catch (Exception e) {
                updateStatus = e.getMessage();
            }
            result.put(config.getIdValue(), updateStatus);
        }
        return result;
    }

    // type helper classes for JSON mapper
    private static class BusUpdateRequest extends UpdateRequest<BusConfig1> {}
    private static class UserUpdateRequest extends UpdateRequest<User> {}
}
