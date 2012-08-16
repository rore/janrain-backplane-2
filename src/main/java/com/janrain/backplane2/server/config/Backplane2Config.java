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

package com.janrain.backplane2.server.config;

import com.janrain.backplane.server.config.BpServerConfig;
import com.janrain.backplane.server.utils.BackplaneSystemProps;
import com.janrain.backplane2.server.BackplaneServerException;
import com.janrain.backplane2.server.V2MessageProcessor;
import com.janrain.backplane2.server.dao.DAOFactory;
import com.janrain.cache.CachedL1;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.util.AwsUtility;
import com.janrain.commons.util.InitSystemProps;
import com.janrain.crypto.HmacHashUtils;
import com.netflix.curator.framework.CuratorFramework;
import com.netflix.curator.framework.CuratorFrameworkFactory;
import com.netflix.curator.framework.recipes.leader.LeaderSelector;
import com.netflix.curator.framework.recipes.leader.LeaderSelectorListener;
import com.netflix.curator.retry.ExponentialBackoffRetry;
import com.yammer.metrics.Metrics;
import com.yammer.metrics.core.MetricName;
import com.yammer.metrics.core.MetricsRegistry;
import com.yammer.metrics.reporting.ConsoleReporter;
import com.yammer.metrics.reporting.GraphiteReporter;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.springframework.context.annotation.Scope;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.inject.Inject;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Properties;
import java.util.TimeZone;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;


/**
 * Holds configuration settings for the Backplane server
 * 
 * @author Jason Cowley, Johnny Bufu
 */
@Scope(value="singleton")
public class Backplane2Config {

    // - PUBLIC

    // http://fahdshariff.blogspot.ca/2010/08/dateformat-with-multiple-threads.html
    public static final ThreadLocal<DateFormat> ISO8601 = new ThreadLocal<DateFormat>() {
        @Override
        protected DateFormat initialValue() {
            return new SimpleDateFormat("yyyy-MM-dd'T'HH:mm:ss.SSS'Z'") {{
                setTimeZone(TimeZone.getTimeZone("GMT"));
            }};
        }
    };

    public void checkAdminAuth(String user, String password) throws AuthException {
        checkAdminAuth(getAdminAuthTableName(), user, password);
    }

    public String getTableName(SimpleDBTables table) {
        return Backplane2Config.this.bpInstanceId + table.getTableSuffix();
    }

    public enum SimpleDBTables {

        BP_SERVER_CONFIG("_bpserverconfig"),
        BP_ADMIN_AUTH("_Admin"),
        BP_BUS_CONFIG("_v2_busconfig"),
        BP_BUS_OWNERS("_v2_bus_owners"),
        BP_CLIENTS("_v2_clients"),
        BP_MESSAGES("_v2_messages"),
        BP_SAMPLES("_samples"),
        BP_METRICS("_metrics"),
        BP_METRIC_AUTH("_bpMetricAuth"),
        BP_GRANT("_v2_grants"),
        BP_ACCESS_TOKEN("_v2_accessTokens"),
        BP_REVOKED_TOKEN("_v2_revokedTokens"),
        BP_AUTH_SESSION("_v2_authSessions"),
        BP_AUTHORIZATION_REQUEST("_v2_authorizationRequests"),
        BP_AUTHORIZATION_DECISION_KEY("_v2_authorizationDecisions");

        public String getTableSuffix() {
            return tableSuffix;
        }

        // - PRIVATE

        private String tableSuffix;

        private SimpleDBTables(String tableSuffix) {
            this.tableSuffix = tableSuffix;
        }
    }



    /**
	 * @return the debugMode
	 */
	public boolean isDebugMode() {
        return Boolean.valueOf(cachedGet(BpServerConfig.Field.DEBUG_MODE));
	}

    /**
     * @return the server default max message value per channel
     * @throws SimpleDBException
     */
    public long getDefaultMaxMessageLimit() {
        Long max = Long.valueOf(cachedGet(BpServerConfig.Field.DEFAULT_MESSAGES_MAX));
        return max == null ? Backplane2Config.BP_MAX_MESSAGES_DEFAULT : max;
    }

    public long getMaxTokenCacheBytes() {
        try {
            Long max = Long.valueOf(cachedGet(BpServerConfig.Field.TOKEN_CACHE_MAX_MB));
            return max == null ? 0L : max * 1024 * 1024;
        } catch (NumberFormatException nfe) {
            logger.error("Invalid message cache size value: " + nfe.getMessage(), nfe);
            return 0L;
        }
    }

    public Exception getDebugException(Exception e) {
        return isDebugMode() ? e: null;
     }

    public String getInstanceId() {
        return bpInstanceId;
    }

    public String getBuildVersion() {
        return buildProperties.getProperty(BUILD_VERSION_PROPERTY);
    }

    /**
     * Retrieve the server instance id Amazon assigned
     * @return
     */

    public static String getEC2InstanceId() {
        return EC2InstanceId;
    }

    // - PACKAGE


    Backplane2Config(String instanceId) {
        this.bpInstanceId = instanceId;
    }

    /**
     * Load system property
     * @param propParamName
     * @return
     */

    static String getAwsProp(String propParamName) {
        String result = System.getProperty(propParamName);
        if (StringUtils.isBlank(result)) {
            throw new RuntimeException("Required system property configuration missing: " + propParamName);
        }
        return result;
    }

    // - PRIVATE

    private static final Logger logger = Logger.getLogger(Backplane2Config.class);

    private static final String BUILD_PROPERTIES = "/build.properties";
    private static final String BUILD_VERSION_PROPERTY = "build.version";
    private static final Properties buildProperties = new Properties();

    private static final String BP_CONFIG_ENTRY_NAME = "bpserverconfig";
    private static final long BP_MAX_MESSAGES_DEFAULT = 100;
    //private static final long MESSAGE_CACHE_UPDATE_INTERVAL_MILLISECONDS = 300;
    private static final long TOKEN_CACHE_REVOKE_CLEANUP_INTERVAL_SECONDS = 5;

    private final String bpInstanceId;
    private ScheduledExecutorService cleanup;
    //private ExecutorService messageCacheUpdater;
    private ExecutorService tokenCacheCleanup;
    private ExecutorService pingRedis;


    // Amazon specific instance-id value
    private static String EC2InstanceId = AwsUtility.retrieveEC2InstanceId();

    private final com.yammer.metrics.core.Timer v2CleanupTimer =
        com.yammer.metrics.Metrics.newTimer(new MetricName(BackplaneSystemProps.getMachineName(), this.getClass().getName(), "cleanup_messages_time"), TimeUnit.MILLISECONDS, TimeUnit.MINUTES);

    @SuppressWarnings({"UnusedDeclaration"})
    private Backplane2Config() {
        this.bpInstanceId = getAwsProp(InitSystemProps.AWS_INSTANCE_ID);

        ConsoleReporter.enable(10, TimeUnit.MINUTES);

        // Dump metrics to graphite server
        String graphiteServer = System.getProperty(BackplaneSystemProps.GRAPHITE_SERVER);
        if (StringUtils.isNotBlank(graphiteServer)) {
            try {
                String args[] = graphiteServer.split(":");
                String server = args[0];
                int port = Integer.parseInt(args[1]);
                GraphiteReporter.enable(10, TimeUnit.SECONDS, server, port);
                logger.info("Graphite server enabled at " + graphiteServer);
            } catch (Exception e) {
                logger.warn("could not enable Graphite from " + graphiteServer + " must be in the form SERVER:PORT");
            }
        }
        try {
            buildProperties.load(Backplane2Config.class.getResourceAsStream(BUILD_PROPERTIES));
            //assert(StringUtils.isNotBlank(getEncryptionKey()));
        } catch (Exception e) {
            String err = "Error loading build properties from " + BUILD_PROPERTIES;
            logger.error(err, e);
            throw new RuntimeException(err, e);
        }

        logger.info("Configured Backplane Server instance: " + bpInstanceId);
    }

    private ScheduledExecutorService createMaintenanceTask() {
        long cleanupIntervalMinutes;
        logger.info("calling v2 createMaintenanceTask()");
        cleanupIntervalMinutes = Long.valueOf(cachedGet(BpServerConfig.Field.CLEANUP_INTERVAL_MINUTES));

        final V2MessageProcessor messageProcessor = new V2MessageProcessor(daoFactory);

        ScheduledExecutorService maintenanceTask = Executors.newScheduledThreadPool(2);

        // one shot thing, but we expect it to run while this node is up
/*        maintenanceTask.schedule(new Runnable() {
            @Override
            public void run() {
                logger.info("creating v2 message processor thread");
                messageProcessor.insertMessages();
            }
        }, 0, TimeUnit.SECONDS);*/

        maintenanceTask.scheduleAtFixedRate(new Runnable() {
            @Override
            public void run() {
                logger.info("creating v2 message cleanup thread");
                messageProcessor.cleanupMessages();
            }
        }, 0, 1, TimeUnit.MINUTES);

        return maintenanceTask;
    }

    private ExecutorService createCacheCleanupTask() {
        ScheduledExecutorService tokenCacheCleaner = Executors.newScheduledThreadPool(1);
        tokenCacheCleaner.scheduleWithFixedDelay(new Runnable() {
            @Override
            public void run() {
                try {
                    daoFactory.getTokenDao().cacheRevokedCleanup();
                } catch (Exception e) {
                    logger.error("Error clearing revoked tokens from cache: " + e.getMessage(), e);
                }
            }
        }, TOKEN_CACHE_REVOKE_CLEANUP_INTERVAL_SECONDS, TOKEN_CACHE_REVOKE_CLEANUP_INTERVAL_SECONDS, TimeUnit.SECONDS);
        return tokenCacheCleaner;
    }

    private ExecutorService createPingTask() {
        ScheduledExecutorService ping = Executors.newScheduledThreadPool(1);
        ping.scheduleWithFixedDelay(new Runnable() {
            @Override
            public void run() {
                com.janrain.redis.Redis.getInstance().ping();
            }
        }, 30, 10, TimeUnit.SECONDS);
        return ping;
    }

    @PostConstruct
    private void init() {

        this.cleanup = createMaintenanceTask();
        //this.cacheUpdater = createCacheUpdaterTask();

        /*for(SimpleDBTables table : EnumSet.allOf(SimpleDBTables.class)) {
            superSimpleDb.checkDomain(getTableName(table));
        }*/

        this.tokenCacheCleanup = createCacheCleanupTask();
        this.pingRedis = createPingTask();

        try {
            String zkServerConfig = System.getProperty("ZOOKEEPER_SERVERS");
            if (StringUtils.isEmpty(zkServerConfig)) {
                logger.error("Cannot find configuration entry for ZooKeeper server");
                System.exit(1);
            }
            CuratorFramework client = CuratorFrameworkFactory.newClient(zkServerConfig, new ExponentialBackoffRetry(50, 20));
            client.start();
            LeaderSelector leaderSelector = new LeaderSelector(client, "/v2_worker", new V2MessageProcessor(daoFactory));
            leaderSelector.start();
            com.janrain.redis.Redis.getInstance().setActiveRedisInstance(client);
        } catch (Exception e) {
            logger.error(e);
        }

    }

    @PreDestroy
    private void cleanup() {
        Metrics.shutdown();
        shutdownExecutor(cleanup);
        shutdownExecutor(tokenCacheCleanup);
        shutdownExecutor(pingRedis);
    }

    private void shutdownExecutor(ExecutorService executor) {
        try {
            executor.shutdown();
            if (executor.awaitTermination(10, TimeUnit.SECONDS)) {
                logger.info("Background thread shutdown properly");
            } else {
                executor.shutdownNow();
                if (!executor.awaitTermination(10, TimeUnit.SECONDS)) {
                    logger.error("Background thread did not terminate");
                }
            }
        } catch (InterruptedException e) {
            logger.error("cleanup() threw an exception", e);
            executor.shutdownNow();
            Thread.currentThread().interrupt();
        }
    }

 //   @Inject
    @SuppressWarnings({"UnusedDeclaration"})
   // private SuperSimpleDB superSimpleDb;

    @Inject
    private DAOFactory daoFactory;

    private String cachedGet(BpServerConfig.Field property) {

        try {

            BpServerConfig bpServerConfigCache = (BpServerConfig) CachedL1.getInstance().getObject(BpServerConfig.BPSERVER_CONFIG_KEY);
            if (bpServerConfigCache == null) {
                // pull from db if not found in cache
                try {
                    bpServerConfigCache = daoFactory.getConfigDAO().get(BpServerConfig.BPSERVER_CONFIG_KEY);
                } catch (Exception e) {
                    // if we get an error from the db, create a new object
                }

                if (bpServerConfigCache == null) {
                    // no instance found in cache or the db, so let's use the default record
                    bpServerConfigCache = new BpServerConfig();
                }
                // add it to the L1 cache
                CachedL1.getInstance().setObject(BpServerConfig.BPSERVER_CONFIG_KEY, -1, bpServerConfigCache);
            }

            return bpServerConfigCache.get(property);

        } catch (Exception e) {
            logger.error(e);
            return null;
        }

    }

    private String getBpServerConfigTableName() {
        return bpInstanceId + SimpleDBTables.BP_SERVER_CONFIG.getTableSuffix();
    }

    private String getAdminAuthTableName() {
        return bpInstanceId + SimpleDBTables.BP_ADMIN_AUTH.getTableSuffix();
    }

    private String getMetricAuthTableName() {
        return bpInstanceId + SimpleDBTables.BP_METRIC_AUTH.getTableSuffix();
    }

    private Long getMaxCacheAge() {
        return Long.valueOf(cachedGet(BpServerConfig.Field.CONFIG_CACHE_AGE_SECONDS));
    }

    public void checkAdminAuth(String authTable, String user, String password) throws AuthException {
        try {
            //User userEntry = superSimpleDb.retrieve(authTable, User.class, user);
            User userEntry = daoFactory.getAdminDAO().get(user);
            String authKey = userEntry == null ? null : userEntry.get(User.Field.PWDHASH);
            if ( ! HmacHashUtils.checkHmacHash(password, authKey) ) {
                logger.error("User " + user + " not authorized in " + authTable);
                throw new AuthException("Access denied");
            }
        } catch (BackplaneServerException e) {
            logger.error("Error authenticating user " + user + " : " + e.getMessage(), getDebugException(e));
            throw new AuthException("User " + user + " not authorized in " + authTable + " , " + e.getMessage(), getDebugException(e));
        }
    }
}
