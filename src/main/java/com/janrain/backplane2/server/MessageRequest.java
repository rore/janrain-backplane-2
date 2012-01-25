package com.janrain.backplane2.server;

import com.janrain.backplane2.server.dao.DaoFactory;
import com.janrain.commons.supersimpledb.SimpleDBException;
import org.apache.commons.lang.StringUtils;
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


}
