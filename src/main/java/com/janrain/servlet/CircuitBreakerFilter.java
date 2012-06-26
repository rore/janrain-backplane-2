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

import com.janrain.backplane2.server.InvalidRequestException;
import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.cache.Cached;
import com.yammer.metrics.Metrics;
import com.yammer.metrics.core.Meter;
import org.apache.log4j.Logger;

import javax.servlet.*;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.io.OutputStream;
import java.util.concurrent.TimeUnit;

/**
 * @author Tom Raney
 */
public class CircuitBreakerFilter implements Filter {
    @Override
    public void init(FilterConfig filterConfig) throws ServletException {
        logger.info("CircuitBreakerFilter initialized.");
    }

    @Override
    public void doFilter(ServletRequest request, ServletResponse response, FilterChain chain) throws IOException, ServletException {

        //TODO: need to work out a trigger
        if (false)  {
            HttpServletResponse httpResponse = (HttpServletResponse) response;
            OutputStream stream = httpResponse.getOutputStream();
            String msg = "{\"error\":\"Backplane server unavailable\"}";
            stream.write(msg.getBytes());
            logger.warn("Aborted due to circuit breaker");
            // don't pass on to additional filters
        }
    }

    @Override
    public void destroy() {}

    // - PRIVATE

    private static final Logger logger = Logger.getLogger(CircuitBreakerFilter.class);

}
