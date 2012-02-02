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

package com.janrain.backplane2.server;

import com.janrain.backplane2.server.dao.DaoFactory;
import com.janrain.commons.supersimpledb.SimpleDBException;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.util.HashMap;

/**
 * @author Tom Raney
 */
public class MessageRequest {

    final String tokenString;
    final String callback;
    Token token;

    private static final String ERR_MSG_FIELD = "error";
    private static final String ERR_MSG_DESCRIPTION = "error_description";

    public MessageRequest(DaoFactory daoFactory, String tokenString, String callback) {
        this.tokenString = tokenString;
        this.callback = callback;

        try {
            setToken(daoFactory.getTokenDao().retrieveToken(tokenString));
        } catch (SimpleDBException e) {
            // do nothing for now
            logger.debug("failed to load token '" + tokenString + "'", e);
        }
    }



    private void setToken(Token token) {
        this.token = token;
    }

    public Token getToken() {
        return this.token;
    }


    public HashMap<String, Object> validate() {

        if (token == null || token.isExpired()) {
            return error("Not authorized",null);
        }

        if (StringUtils.isNotEmpty(callback)) {
            if (!callback.matches("[a-zA-Z0-9]*")) {
                return error("invalid_request", "Callback parameter value is malformed");
            }
        }

        // is the token properly scoped for this message id?

        return new HashMap<String, Object>();
    }


    public HashMap<String, Object> error(@NotNull final String error, final String description) {
        return new HashMap<String, Object>() {{
            put(ERR_MSG_FIELD, error);
            if (description != null) {
                put(ERR_MSG_DESCRIPTION, description);
            }
        }};
    }

    // - PRIVATE

    private static final Logger logger = Logger.getLogger(MessageRequest.class);


}
