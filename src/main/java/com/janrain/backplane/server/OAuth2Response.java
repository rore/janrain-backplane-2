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

package com.janrain.backplane.server;

import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import com.janrain.crypto.ChannelUtil;

import java.util.Date;
import java.util.HashMap;
import java.util.LinkedHashMap;

/**
 * Response
 * @author Tom Raney
 */
public class OAuth2Response {

    TokenRequest request;
    private TokenDAO tokenDao;

    OAuth2Response(TokenDAO tokenDao, TokenRequest request) {
        this.request = request;
        this.tokenDao = tokenDao;
    }

    public HashMap<String, Object> generateResponse() throws SimpleDBException {
        if (request.grant_type.equals("client_credentials") && request.client_id.equals(Token.ANONYMOUS)) {
            // issue new channel id
            final Token token = new Token(Access.type.REGULAR_TOKEN,  null, new Date(new Date().getTime() + Token.EXPIRES_SECONDS * 1000));
            tokenDao.persistToken(token);
            return new LinkedHashMap<String, Object>() {{
                put("access_token", token.getIdValue());
                put("expires_in", Token.EXPIRES_SECONDS);
                put("token_type", token.getTokenType());
                put("backplane_channel", token.getChannelName());
            }};
        }

        if (request.grant_type.equals("code")) {
            final Token token = new Token(request.getCode());
            tokenDao.persistToken(token);
            return new LinkedHashMap<String, Object>() {{
                put("access_token", token.getIdValue());
                put("token_type", "Bearer");
            }};
        }

        return null;
    }
}
