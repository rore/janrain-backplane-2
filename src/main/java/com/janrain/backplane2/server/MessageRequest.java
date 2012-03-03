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

import javax.servlet.http.HttpServletResponse;
import java.util.EnumSet;

/**
 * @author Tom Raney
 */
public class MessageRequest {

    public MessageRequest(DaoFactory daoFactory, String callback, String tokenString, EnumSet<Token.Source> tokenFoundIn) {
        this.callback = callback;
        this.tokenFoundIn = tokenFoundIn;
        try {
            this.token = daoFactory.getTokenDao().retrieveToken(tokenString);
        } catch (SimpleDBException e) {
            // do nothing for now
            logger.debug("failed to load token '" + tokenString + "'", e);
        }
        validate();
    }

    public Token getToken() {
        return this.token;
    }

    // - PRIVATE

    private static final Logger logger = Logger.getLogger(MessageRequest.class);

    private final String callback;
    private Token token;
    private final EnumSet<Token.Source> tokenFoundIn;

    private void validate() throws InvalidRequestException {

        if (StringUtils.isNotEmpty(callback)) {
            if (!callback.matches("[\\._a-zA-Z0-9]*")) {
                throw new InvalidRequestException("invalid_request", "Callback parameter value is malformed");
            }
        }

        if (token == null || token.isExpired()) {
            throw new InvalidRequestException("Not authorized", HttpServletResponse.SC_FORBIDDEN);
        }
        
        if (! token.isAllowedSources(tokenFoundIn)) {
            throw new InvalidRequestException("invalid_request", "token source(s) not allowed: " + tokenFoundIn);
        }

        // is the token properly scoped for this message id?
    }
}
