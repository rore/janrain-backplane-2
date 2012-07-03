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

package com.janrain.backplane2.server.dao;

import com.janrain.backplane2.server.BackplaneServerException;
import com.janrain.backplane2.server.BackplaneMessage;
import com.janrain.backplane2.server.MessagesResponse;
import com.janrain.backplane2.server.Token;
import com.janrain.oauth2.TokenException;
import com.yammer.metrics.Metrics;
import com.yammer.metrics.core.Histogram;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import javax.servlet.http.HttpServletResponse;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;
import java.util.Set;
import java.util.concurrent.Callable;
import java.util.concurrent.TimeUnit;

import static com.janrain.backplane2.server.BackplaneMessage.Field.*;
import static com.janrain.backplane2.server.config.Backplane2Config.SimpleDBTables.BP_MESSAGES;
import static com.janrain.backplane2.server.config.BusConfig2.Field.*;

/**
 * @author Tom Raney, Johnny Bufu
 */
public interface BackplaneMessageDAO extends DAO<BackplaneMessage> {

    BackplaneMessage getLatestMessage() throws BackplaneServerException;
    @NotNull BackplaneMessage retrieveBackplaneMessage(@NotNull final String messageId, @NotNull Token token) throws BackplaneServerException, TokenException;
    boolean isChannelFull(String channel) throws BackplaneServerException;
    boolean canTake(String channel, int msgPostCount) throws BackplaneServerException;
    long countMessages() throws BackplaneServerException;

    /**
     * Retrieve all messages by per scope in the provided bpResponse object.
     * Guaranteed to delivery results in order of message ID.
     *
     * @param bpResponse backplane message response
     * @param token access token to be used for message retrieval
     */
    void retrieveMesssagesPerScope(@NotNull final MessagesResponse bpResponse, @NotNull final Token token) throws BackplaneServerException;
    List<BackplaneMessage> retrieveMessagesNoScope(String sinceIso8601timestamp) throws BackplaneServerException;
    List<BackplaneMessage> retrieveMessagesByChannel(String channel) throws BackplaneServerException;
    void deleteExpiredMessages() throws BackplaneServerException;

}
