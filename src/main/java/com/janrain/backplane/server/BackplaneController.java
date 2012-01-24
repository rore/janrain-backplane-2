/*
 * Copyright 2011 Janrain, Inc.
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

import com.janrain.backplane.server.config.AuthException;
import com.janrain.backplane.server.config.BackplaneConfig;
import com.janrain.backplane.server.dao.BackplaneMessageDAO;
import com.janrain.backplane.server.dao.DaoFactory;
import com.janrain.backplane.server.metrics.MetricsAccumulator;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.yammer.metrics.Metrics;
import com.yammer.metrics.core.HistogramMetric;
import com.yammer.metrics.core.MeterMetric;
import com.yammer.metrics.core.TimerMetric;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.codehaus.jackson.map.ObjectMapper;
import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.*;
import org.springframework.web.servlet.ModelAndView;

import javax.inject.Inject;
import javax.servlet.http.HttpServletRequest;
import javax.servlet.http.HttpServletResponse;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.concurrent.TimeUnit;

/**
 * Backplane API implementation.
 *
 * @author Johnny Bufu
 */
@Controller
@RequestMapping(value="/*")
@SuppressWarnings({"UnusedDeclaration"})
public class BackplaneController {

    // - PUBLIC

    @RequestMapping(value = "/", method = { RequestMethod.GET, RequestMethod.HEAD })
    public ModelAndView greetings(HttpServletRequest request, HttpServletResponse response) {
        if (RequestMethod.HEAD.toString().equals(request.getMethod())) {
            response.setContentLength(0);
        }
        return new ModelAndView("welcome");
    }

    /**
     * Handle dynamic discovery of this server's registration endpoint
     * @return
     */
    @RequestMapping(value = "/.well-known/host-meta", method = { RequestMethod.GET})
    public ModelAndView xrds(HttpServletRequest request, HttpServletResponse response) {

        ModelAndView view = new ModelAndView("xrd");
        view.addObject("host", "http://" + request.getServerName());
        view.addObject("secureHost", "https://" + request.getServerName());
        return view;
    }

    /**
     * The OAuth "Token Endpoint" is used to obtain an access token to be used
     * for retrieving messages from the Get Messages endpoint.
     *
     * @param request
     * @param response
     * @param scope     optional
     * @param callback  required
     * @return
     * @throws AuthException
     * @throws SimpleDBException
     * @throws BackplaneServerException
     */

    @RequestMapping(value = "/token", method = { RequestMethod.GET})
    @ResponseBody
    public HashMap<String,Object> getToken(HttpServletRequest request, HttpServletResponse response,
                                           @RequestParam(value = "scope", required = false) String scope,
                                           @RequestParam(required = true) String callback)
            throws AuthException, SimpleDBException, BackplaneServerException {

        TokenRequest tokenRequest = new TokenRequest("anonymous", "client_credentials", scope, callback);

        HashMap errors = tokenRequest.validate();

        if (!errors.isEmpty()) {
            response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
            return errors;
        }

        HashMap<String, Object> hash;

        try {
            hash= new OAuth2Response(tokenRequest, daoFactory).generateResponse();
        } catch (final BackplaneServerException bpe) {
            return new HashMap<String,Object>() {{
                put(ERR_MSG_FIELD, bpe.getMessage());
            }};

        }

        if (StringUtils.isBlank(callback)) {
            throw new IllegalArgumentException("Callback cannot be blank");
        } else {
            response.setContentType("application/x-javascript");
            try {
                String responseBody = callback + "(" + new String(new ObjectMapper().writeValueAsString(hash) + ")");
                response.getWriter().print(responseBody);
                return null;
            } catch (IOException e) {
                String errMsg = "Error converting frames to JSON: " + e.getMessage();
                logger.error(errMsg, bpConfig.getDebugException(e));
                throw new BackplaneServerException(errMsg, e);
            }
        }
    }

    /**
     * The OAuth "Token Endpoint" is used to obtain an access token to be used
     * for retrieving messages from the Get Messages endpoint.
     *
     * @param request
     * @param response
     * @param client_id
     * @param grant_type
     * @param redirect_uri
     * @param code
     * @param client_secret
     * @param scope
     * @return
     * @throws AuthException
     * @throws SimpleDBException
     * @throws BackplaneServerException
     */

    @RequestMapping(value = "/token", method = { RequestMethod.POST})
    @ResponseBody
    public HashMap<String,Object> token(HttpServletRequest request, HttpServletResponse response,
                                        @RequestParam(value = "client_id", required = false) String client_id,
                                        @RequestParam(value = "grant_type", required = false) String grant_type,
                                        @RequestParam(value = "redirect_uri", required = false) String redirect_uri,
                                        @RequestParam(value = "code", required = false) String code,
                                        @RequestParam(value = "client_secret", required = false) String client_secret,
                                        @RequestParam(value = "scope", required = false) String scope)
            throws AuthException, SimpleDBException, BackplaneServerException {

        TokenRequest tokenRequest = new TokenRequest(daoFactory, client_id, grant_type, redirect_uri,
                                                        code, client_secret, scope, null);

        HashMap errors = tokenRequest.validate();
        if (!errors.isEmpty()) {
            response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
            return errors;
        }

        //TODO: we need to push the "scope" parameter back in the response, if it differs from
        // the requested scope.

        try {
            return new OAuth2Response(tokenRequest, daoFactory).generateResponse();
        } catch (final BackplaneServerException bpe) {
            return new HashMap<String,Object>() {{
                put(ERR_MSG_FIELD, bpe.getMessage());
            }};

        }

    }

    /**
     * Retrieve messages from the server.
     * @param request
     * @param response
     * @return
     */

    @RequestMapping(value = "/messages", method = { RequestMethod.GET})
    public @ResponseBody HashMap<String,Object> messages(HttpServletRequest request, HttpServletResponse response,
                                @RequestParam(value = "access_token", required = false) String access_token,
                                @RequestParam(value = "block", defaultValue = "0", required = false) Integer block,
                                @RequestParam(required = false) String callback,
                                @RequestParam(value = "since", required = false) String since)
            throws SimpleDBException, BackplaneServerException {

        //TODO: add support for block?

        MessageRequest messageRequest = new MessageRequest(daoFactory, access_token, callback);

        HashMap<String, Object> errors = messageRequest.validate();
        if (!errors.isEmpty()) {
            return errors;
        }

        List<BackplaneMessage> messages =
                daoFactory.getBackplaneMessageDAO().retrieveAllMesssagesPerScope(messageRequest.getToken().getScope(), since);

        if (messages.isEmpty()) {
            try {
                response.getWriter().print("{}");
            } catch (IOException e) {
                logger.error(e);
                throw new BackplaneServerException("A server error has occurred");
            }
            return null;
        }

        String nextUrl = "https://" + request.getServerName() + "/v2/messages?since=" + messages.get(messages.size()-1).getIdValue();
        List<Map<String,Object>> frames = new ArrayList<Map<String, Object>>();

        for (BackplaneMessage message : messages) {
            frames.add(message.asFrame(request.getServerName(), messageRequest.getToken().isPrivileged()));

            // verify the proper permissions
            if (messageRequest.getToken().isPrivileged()) {
                if (!messageRequest.getToken().isAllowedBus(message.get(BackplaneMessage.Field.BUS))) {
                    // For the privileged request, make sure the message bus matches a bus in the token
                    response.setStatus(HttpServletResponse.SC_FORBIDDEN);
                    return new HashMap<String,Object>() {{
                        put(ERR_MSG_FIELD, "Forbidden");
                    }};
                }
            } else {
                if (!message.get(BackplaneMessage.Field.CHANNEL).equals(messageRequest.getToken().getChannelName())) {
                    response.setStatus(HttpServletResponse.SC_FORBIDDEN);
                    return new HashMap<String,Object>() {{
                        put(ERR_MSG_FIELD, "Forbidden");
                    }};
                }
            }
        }

        HashMap<String, Object> hash = new HashMap<String, Object>();
        hash.put("nextURL", nextUrl);
        hash.put("messages", frames);

        if (StringUtils.isBlank(callback)) {
            response.setContentType("application/json");
            return hash;
        } else {
            response.setContentType("application/x-javascript");
            try {
                String responseBody = callback + "(" + new String(new ObjectMapper().writeValueAsString(hash) + ")");

                response.getWriter().print(responseBody);

                return null;
            } catch (IOException e) {
                String errMsg = "Error converting frames to JSON: " + e.getMessage();
                logger.error(errMsg, bpConfig.getDebugException(e));
                throw new BackplaneServerException(errMsg, e);
            }
        }
    }



    /**
     * Retrieve a single message from the server.
     * @param request
     * @param response
     * @return
     */

    @RequestMapping(value = "/message/{msg_id}", method = { RequestMethod.GET})
    public @ResponseBody HashMap<String,Object> message(HttpServletRequest request, HttpServletResponse response,
                                                        @PathVariable String msg_id,
                                                        @RequestParam(value = "access_token", required = false) String access_token,
                                                        @RequestParam(required = false) String callback)
            throws BackplaneServerException {

        MessageRequest messageRequest = new MessageRequest(daoFactory, access_token, callback);

        HashMap error = messageRequest.validate();
        if (!error.isEmpty()) {
            response.setStatus(HttpServletResponse.SC_FORBIDDEN);
            return error;
        }

        BackplaneMessage message = null;

        try {
            message = daoFactory.getBackplaneMessageDAO().retrieveBackplaneMessage(msg_id);
        } catch (SimpleDBException e) {
            logger.info("Could not find message " + msg_id,e);
        }

        if (message == null) {
            response.setStatus(HttpServletResponse.SC_NOT_FOUND);
            return new HashMap<String,Object>() {{
                put(ERR_MSG_FIELD, "Message not found");
            }};
        } else {
            // verify the proper permissions
            if (messageRequest.getToken().isPrivileged()) {
                if (!messageRequest.getToken().isAllowedBus(message.get(BackplaneMessage.Field.BUS))) {
                    // For the privileged request, make sure the message bus matches a bus in the token
                    response.setStatus(HttpServletResponse.SC_FORBIDDEN);
                    return new HashMap<String,Object>() {{
                        put(ERR_MSG_FIELD, "Forbidden");
                    }};
                }
            } else {
                if (!message.get(BackplaneMessage.Field.CHANNEL).equals(messageRequest.getToken().getChannelName())) {
                    response.setStatus(HttpServletResponse.SC_FORBIDDEN);
                    return new HashMap<String,Object>() {{
                        put(ERR_MSG_FIELD, "Forbidden");
                    }};
                }
            }
        }

        error = messageRequest.validate();
        if (!error.isEmpty()) {
            response.setStatus(HttpServletResponse.SC_FORBIDDEN);
            return error;
        }

        if (StringUtils.isBlank(callback)) {
            response.setContentType("application/json");
            return message.asFrame(request.getServerName(), messageRequest.getToken().isPrivileged());
        } else {
            response.setContentType("application/x-javascript");
            try {
                String responseBody = callback + "(" +
                        new String(new ObjectMapper().writeValueAsString(message.asFrame(request.getServerName(), messageRequest.getToken().isPrivileged())) + ")");

                response.getWriter().print(responseBody);

                return null;
            } catch (IOException e) {
                String errMsg = "Error converting frames to JSON: " + e.getMessage();
                logger.error(errMsg, bpConfig.getDebugException(e));
                throw new BackplaneServerException(errMsg, e);
            }
        }



    }

    /**
     * Publish messages to the Backplane.
     * @param request
     * @param response
     * @return
     */

    @RequestMapping(value = "/messages", method = { RequestMethod.POST})
    public @ResponseBody HashMap<String,Object>  postMessages(HttpServletRequest request, HttpServletResponse response,
                                                              @RequestBody Map<String,List<Map<String,Object>>> messages,
                                                              @RequestParam(value = "access_token", required = false) String access_token,
                                                              @RequestParam(required = false) String callback,
                                                              @RequestParam(required = false) String since) throws BackplaneServerException, SimpleDBException {

        Token token = null;

        if (StringUtils.isEmpty(access_token)) {
            response.setStatus(HttpServletResponse.SC_FORBIDDEN);
            return new HashMap<String,Object>() {{
                put(ERR_MSG_FIELD, "Forbidden");
            }};
        }

        try {
            token = daoFactory.getTokenDao().retrieveToken(access_token);
        } catch (SimpleDBException e) {
            response.setStatus(HttpServletResponse.SC_FORBIDDEN);
            return new HashMap<String,Object>() {{
                put(ERR_MSG_FIELD, "Invalid token");
            }};
        }

        if (!token.isPrivileged()) {
            response.setStatus(HttpServletResponse.SC_FORBIDDEN);
            return new HashMap<String,Object>() {{
                put(ERR_MSG_FIELD, "Forbidden");
            }};
        }

        //TODO: add logic to verify the message cap per channel has not been exceeded
        // and confirm what to do if the Nth message went over the limit but the first N-1
        // messages succeeded in one request...

        // analyze each message for proper bus
        List<Map<String,Object>> msgs = messages.get("messages");
        for(Map<String,Object> messageData : msgs) {
            BackplaneMessage message = new BackplaneMessage(messageData);
            if (!token.isAllowedBus(message.get(BackplaneMessage.Field.BUS))) {
                response.setStatus(HttpServletResponse.SC_FORBIDDEN);
                return new HashMap<String,Object>() {{
                    put(ERR_MSG_FIELD, "Invalid bus in message");
                }};
            }

            if (daoFactory.getTokenDao().retrieveTokenByChannel(message.get(BackplaneMessage.Field.CHANNEL)) == null) {
                response.setStatus(HttpServletResponse.SC_FORBIDDEN);
                return new HashMap<String,Object>() {{
                    put(ERR_MSG_FIELD, "Invalid channel in message");
                }};
            }
        }

        // do it all again and store the messages in the db
        BackplaneMessageDAO bmd = daoFactory.getBackplaneMessageDAO();
        for(Map<String,Object> messageData : msgs) {
            bmd.persistBackplaneMessage(new BackplaneMessage(messageData));
        }

        response.setStatus(HttpServletResponse.SC_CREATED);
        return null;

    }

    /**
     * Handle auth errors
     */
    @ExceptionHandler
    @ResponseBody
    public Map<String, String> handle(final AuthException e, HttpServletResponse response) {
        logger.error("Backplane authentication error: " + bpConfig.getDebugException(e));
        response.setStatus(HttpServletResponse.SC_UNAUTHORIZED);
        return new HashMap<String,String>() {{
            put(ERR_MSG_FIELD, e.getMessage());
        }};
    }

    /**
     * Handle all other errors
     */
    @ExceptionHandler
    @ResponseBody
    public Map<String, String> handle(final Exception e, HttpServletResponse response) {
        logger.error("Error handling backplane request", bpConfig.getDebugException(e));
        response.setStatus(HttpServletResponse.SC_BAD_REQUEST);
        return new HashMap<String,String>() {{
            try {
                put(ERR_MSG_FIELD, bpConfig.isDebugMode() ? e.getMessage() : "Error processing request.");
            } catch (SimpleDBException e1) {
                put(ERR_MSG_FIELD, "Error processing request.");
            }
        }};
    }


    /*
    public static String randomString(int length) {
        byte[] randomBytes = new byte[length];
        // the base64 character set per RFC 4648 with last two members '-' and '_' removed due to possible
        // compatibility issues.
        byte[] digits = {'A','B','C','D','E','F','G','H','I','J','K','L','M','N','O','P','Q','R','S','T',
                         'U','V','W','X','Y','Z','a','b','c','d','e','f','g','h','i','j','k','l','m','n',
                         'o','p','q','r','s','t','u','v','w','x','y','z','0','1','2','3','4','5','6','7',
                         '8','9'};
        random.nextBytes(randomBytes);
        for (int i = 0; i < length; i++) {
            byte b = randomBytes[i];
            int c = Math.abs(b % digits.length);
            randomBytes[i] = digits[c];
        }
        try {
            return new String(randomBytes, "US-ASCII");
        }
        catch (UnsupportedEncodingException e) {
            logger.error("US-ASCII character encoding not supported", e); // shouldn't happen
            return null;
        }
    }
    */
    
    // - PRIVATE

    private static final Logger logger = Logger.getLogger(BackplaneController.class);

    private static final String ERR_MSG_FIELD = "error";
    private static final String ERR_MSG_DESCRIPTION = "error_description";

    private final MeterMetric posts =
            Metrics.newMeter(BackplaneController.class, "post", "posts", TimeUnit.MINUTES);

    private final MeterMetric channelGets =
            Metrics.newMeter(BackplaneController.class, "channel_get", "channel_gets", TimeUnit.MINUTES);
    private final MeterMetric channelGetsSticky = Metrics.newMeter(BackplaneController.class, "channel_gets_sticky", "channel_gets_sticky", TimeUnit.MINUTES);


    private final MeterMetric busGets =
            Metrics.newMeter(BackplaneController.class, "bus_get", "bus_gets", TimeUnit.MINUTES);
    private final MeterMetric busGetsSticky = Metrics.newMeter(BackplaneController.class, "bus_gets_sticky", "bus_gets_sticky", TimeUnit.MINUTES);

    private final TimerMetric getMessagesTime =
            Metrics.newTimer(BackplaneController.class, "get_messages_time", TimeUnit.MILLISECONDS, TimeUnit.MINUTES);

    private final HistogramMetric payLoadSizesOnGets = Metrics.newHistogram(BackplaneController.class, "payload_sizes_gets");

    private final HistogramMetric messagesPerChannel = Metrics.newHistogram(BackplaneController.class, "messages_per_channel");

    @Inject
    private BackplaneConfig bpConfig;

    @Inject
    private DaoFactory daoFactory;

    @Inject
    private MetricsAccumulator metricAccumulator;

    //private static final Random random = new SecureRandom();

    private void authError(String errMsg) throws AuthException {
        logger.error(errMsg);
        try {
            throw new AuthException("Access denied. " + (bpConfig.isDebugMode() ? errMsg : ""));
        } catch (Exception e) {
            throw new AuthException("Access denied.");
        }
    }

    private String paddedResponse(String callback, String s) {
        if (StringUtils.isBlank(callback)) {
            throw new IllegalArgumentException("Callback cannot be blank.");
        }
        StringBuilder result = new StringBuilder(callback);
        result.append("(").append(s).append(")");
        return result.toString();
    }


}
