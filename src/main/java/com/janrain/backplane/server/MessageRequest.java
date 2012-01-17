package com.janrain.backplane.server;

import com.janrain.commons.supersimpledb.SimpleDBException;
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

    public MessageRequest(String tokenString, String callback) {
        this.tokenString = tokenString;
        this.callback = callback;
    }

    public void setToken(Token token) {
        this.token = token;
    }

    public Token getToken() {
        return this.token;
    }

    public HashMap<String, Object> validate() {

        if (token == null) {
            return error("Not authorized",null);
        }

        // is the token properly scoped for this message id?


        return null;



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
