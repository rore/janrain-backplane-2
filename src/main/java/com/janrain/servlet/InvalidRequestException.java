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

package com.janrain.servlet;

import org.jetbrains.annotations.Nullable;
import org.springframework.http.HttpStatus;
import org.springframework.web.bind.annotation.ResponseStatus;

import javax.servlet.http.HttpServletResponse;

@ResponseStatus(value=HttpStatus.BAD_REQUEST)
public class InvalidRequestException extends RuntimeException {

    public InvalidRequestException(String message) {
        this(message, HttpServletResponse.SC_BAD_REQUEST, null);
    }

    public InvalidRequestException(String message, int httpResponseCode) {
        this(message, httpResponseCode, null);
    }

    public InvalidRequestException(String message, @Nullable String errorDescription) {
        this(message, HttpServletResponse.SC_BAD_REQUEST, errorDescription);
    }

    public InvalidRequestException(String message, int httpResponseCode, @Nullable String errorDescription) {
        super(message);
        this.httpResponseCode = httpResponseCode;
        this.errorDescription = errorDescription;
    }

    public int getHttpResponseCode() {
        return httpResponseCode;
    }

    public String getErrorDescription() {
        return errorDescription;
    }

    // - PRIVATE

    private final int httpResponseCode;
    private final String errorDescription;
}