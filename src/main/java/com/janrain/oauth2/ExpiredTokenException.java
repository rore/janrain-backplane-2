package com.janrain.oauth2;

import org.jetbrains.annotations.Nullable;

import javax.servlet.http.HttpServletResponse;

/**
 * OAuth2 /token processing errors.
 * If not supplied on constructor, defaults to:
 * - 400 bad request HTTP response status code
 * - error=invalid_request OAuth2 token error code
 *
 * @author Johnny Bufu
 */
public class ExpiredTokenException extends TokenException {

    public ExpiredTokenException(String message) {
        super(message);
    }

    public ExpiredTokenException(String message, int httpResponseCode) {
        super(message, httpResponseCode);
    }

    public ExpiredTokenException(String oauthErrorCode, String message) {
        super(oauthErrorCode, message);
    }

    public ExpiredTokenException(String oauthErrorCode, String message, Throwable cause) {
        super(oauthErrorCode, message, cause);
    }

    public ExpiredTokenException(String oauthErrorCode, Throwable cause) {
        super(oauthErrorCode, cause);
    }

    public ExpiredTokenException(String oauthErrorCode, String message, int httpResponseCode) {
        super(oauthErrorCode, message, httpResponseCode);
    }

    public ExpiredTokenException(String oauthErrorCode, @Nullable String message, int httpResponseCode, @Nullable Throwable cause) {
        super(oauthErrorCode, message, httpResponseCode, cause);
    }
}
