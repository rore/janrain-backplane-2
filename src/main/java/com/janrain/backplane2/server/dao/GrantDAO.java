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

import com.janrain.backplane2.server.*;
import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import com.janrain.oauth2.TokenException;
import com.yammer.metrics.Metrics;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.jetbrains.annotations.NotNull;
import org.jetbrains.annotations.Nullable;

import java.util.*;
import java.util.concurrent.Callable;
import java.util.concurrent.TimeUnit;

import static com.janrain.backplane2.server.config.Backplane2Config.SimpleDBTables.BP_GRANT;

/**
 * @author Tom Raney
 */

public interface GrantDAO extends DAO<Grant> {

    void update(Grant grant) throws BackplaneServerException, TokenException;
    List<Grant> getByClientId(String clientId) throws BackplaneServerException;
    void deleteByBuses(@NotNull List<String> busesToDelete) throws BackplaneServerException, TokenException;
    void revokeBuses(Set<Grant> grants, String buses) throws BackplaneServerException, TokenException;
}
