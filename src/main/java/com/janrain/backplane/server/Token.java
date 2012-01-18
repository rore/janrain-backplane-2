package com.janrain.backplane.server;

import com.janrain.crypto.ChannelUtil;
import org.apache.commons.lang.StringUtils;
import org.omg.PortableInterceptor.ServerRequestInfo;

import java.util.Date;

/**
 * @author Tom Raney
 */

public class Token extends Access {

    public static final int TOKEN_LENGTH = 20;
    public static final int EXPIRES_SECONDS = 3600;
    public static final String ANONYMOUS = "anonymous";

    /**
     * Empty default constructor for AWS to use.
     * Don't call directly.
     */
    public Token() {};

    Token(String tokenString, type accessType, String authdBusesString, String scopeString, Date expires) throws BackplaneServerException {
        super(tokenString,accessType,authdBusesString,scopeString,expires,accessType == accessType.REGULAR_TOKEN ? true: false);

        if (accessType == type.PRIVILEGED_TOKEN) {
            if (new Scope(scopeString).getBusesInScope().isEmpty()) {
                // if a privileged user has requested a token without specifying a bus in the scope, copy
                // over all authorized buses from the set of authorized buses

                if (StringUtils.isBlank(scopeString)) {
                    scopeString = "";
                }

                scopeString = getEncodedBusesAsString() + " " + scopeString;
                this.setScopeString(scopeString);
            }

            if (!isAllowedBuses(new Scope(this.getScopeString()).getBusesInScope())) {
                throw new BackplaneServerException("Scope request not allowed");
            }
        }

    }

    public Token(type accessType, String buses, String scope, Date expires) throws BackplaneServerException {
        this(ChannelUtil.randomString(TOKEN_LENGTH), accessType, buses, scope, expires);
    }

    public Token(Code code, String scope) throws BackplaneServerException {
        this(ChannelUtil.randomString(TOKEN_LENGTH), type.PRIVILEGED_TOKEN, code.getBusesAsString(), scope, null);
    }

    public String getTokenType() {
        return "Bearer";
    }

    public type getAccessType() {
        return type.valueOf(this.get(Field.TYPE));
    }

    public Scope getScope() throws BackplaneServerException {
        return new Scope(this.get(Field.SCOPE));
    }

    public boolean isPrivileged() {
        return getAccessType() == type.PRIVILEGED_TOKEN;
    }

}
