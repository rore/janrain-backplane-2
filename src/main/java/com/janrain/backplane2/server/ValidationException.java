package com.janrain.backplane2.server;

/**
 * @author Johnny Bufu
 */
public class ValidationException extends Exception {

    public ValidationException(String code, String message) {
        super(message);
        this.code = code;
    }

    public ValidationException(String code, String message, Throwable cause) {
        super(message, cause);
        this.code = code;
    }

    public ValidationException(String code, Throwable cause) {
        super(cause);
        this.code = code;
    }

    public String getCode() {
        return code;
    }

    // - PRIVATE

    private final String code;
}
