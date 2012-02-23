package com.janrain.oauth2;

/**
 * @author Johnny Bufu
 */
public class TokenException extends OAuth2Exception {

    public TokenException(String oauthErrorCode, String message) {
        super(oauthErrorCode, message);
    }

    public TokenException(String oauthErrorCode, String message, Throwable cause) {
        super(oauthErrorCode, message, cause);
    }

    public TokenException(String oauthErrorCode, Throwable cause) {
        super(oauthErrorCode, cause);
    }
}
