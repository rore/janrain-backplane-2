package com.janrain.backplane

/**
 * @author Johnny Bufu
 */
class MessageException(message: String, cause: Throwable = null) extends Exception(message, cause) {}
