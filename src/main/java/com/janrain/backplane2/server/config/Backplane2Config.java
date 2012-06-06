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

import com.janrain.backplane2.server.BackplaneMessage;
import com.janrain.backplane2.server.dao.DaoFactory;
import com.janrain.backplane2.server.dao.MessageCache;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import com.janrain.commons.supersimpledb.message.AbstractNamedMap;
import com.janrain.commons.util.AwsUtility;
import com.janrain.commons.util.InitSystemProps;
import com.janrain.crypto.HmacHashUtils;
import com.janrain.metrics.MetricMessage;
import com.yammer.metrics.Metrics;
import com.yammer.metrics.core.Timer;
import com.yammer.metrics.core.TimerContext;
import com.yammer.metrics.reporting.ConsoleReporter;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.springframework.context.annotation.Scope;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.inject.Inject;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Calendar;
import java.util.EnumSet;
import java.util.Properties;
import java.util.TimeZone;
import java.util.concurrent.*;


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
        checkAuth(getAdminAuthTableName(), user, password);
    }

    public void checkMetricAuth(String user, String password) throws AuthException {
        checkAuth(getMetricAuthTableName(), user, password);
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
	public boolean isDebugMode() throws SimpleDBException {
		return Boolean.valueOf(cachedGet(BpServerProperty.DEBUG_MODE));
	}

    /**
	 * @return the encryptionKey
	 */
	//public String getEncryptionKey() throws SimpleDBException {
	//	return cachedGet(BpServerProperty.ENCRYPTION_KEY);
	//}

    /**
     * @return the server default max message value per channel
     * @throws SimpleDBException
     */
    public long getDefaultMaxMessageLimit() throws SimpleDBException {
        Long max = Long.valueOf(cachedGet(BpServerProperty.DEFAULT_MESSAGES_MAX));
        return max == null ? Backplane2Config.BP_MAX_MESSAGES_DEFAULT : max;
    }

    public long getMaxMessageCacheBytes() {
        try {
            Long max = Long.valueOf(cachedGet(BpServerProperty.MESSAGE_CACHE_MAX_MB));
            return max == null ? 0L : max * 1024 * 1024;
        } catch (NumberFormatException nfe) {
            logger.error("Invalid message cache size value: " + nfe.getMessage(), nfe);
            return 0L;
        } catch (SimpleDBException sdbe) {
            logger.error("Error looking up message cache size: " + sdbe, sdbe);
            return 0L;
        }
    }

    public Exception getDebugException(Exception e) {
        try {
            return isDebugMode() ? e : null;
        } catch (SimpleDBException sdbe) {
            logger.error("Error getting debug mode", sdbe); // shouldn't happen
            return e;
        }
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
    private static final long CACHE_UPDATER_INTERVAL_MILLISECONDS = 300;

    private final String bpInstanceId;
    private ScheduledExecutorService cleanup;
    private ExecutorService cacheUpdater;


    // Amazon specific instance-id value
    private static String EC2InstanceId = "n/a";

    private final com.yammer.metrics.core.Timer v2CleanupTimer =
        com.yammer.metrics.Metrics.newTimer(Backplane2Config.class, "cleanup_messages_time", TimeUnit.MILLISECONDS, TimeUnit.MINUTES);

    private static enum BpServerProperty {
        DEBUG_MODE,
        CONFIG_CACHE_AGE_SECONDS,
        CLEANUP_INTERVAL_MINUTES,
        DEFAULT_MESSAGES_MAX,
        MESSAGE_CACHE_MAX_MB,
        ENCRYPTION_KEY
    }

    @SuppressWarnings({"UnusedDeclaration"})
    private Backplane2Config() {
        this.bpInstanceId = getAwsProp(InitSystemProps.AWS_INSTANCE_ID);
        this.EC2InstanceId = new AwsUtility().retrieveEC2InstanceId();

        ConsoleReporter.enable(5, TimeUnit.MINUTES);

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

    private ScheduledExecutorService createCleanupTask() {
        long cleanupIntervalMinutes;
        logger.info("calling createCleanupTask()");
        try {
            cleanupIntervalMinutes = Long.valueOf(cachedGet(BpServerProperty.CLEANUP_INTERVAL_MINUTES));
        } catch (SimpleDBException e) {
            throw new RuntimeException("Error getting server property " + BpServerProperty.CLEANUP_INTERVAL_MINUTES, e);
        }

        ScheduledExecutorService cleanupTask = Executors.newScheduledThreadPool(1);
        cleanupTask.scheduleAtFixedRate(new Runnable() {
            @Override
            public void run() {

                final TimerContext context = v2CleanupTimer.time();

                try {

                    daoFactory.getBackplaneMessageDAO().deleteExpiredMessages();
                    daoFactory.getTokenDao().deleteExpiredTokens();
                    daoFactory.getAuthSessionDAO().deleteExpiredAuthSessions();
                    daoFactory.getAuthorizationRequestDAO().deleteExpiredAuthorizationRequests();
                    daoFactory.getAuthorizationDecisionKeyDAO().deleteExpiredAuthorizationDecisionKeys();

                } catch (Exception e) {
                    logger.error("Error while cleaning up expired stuff, " + e.getMessage(), e);
                } finally {
                    context.stop();
                }
            }
        }, cleanupIntervalMinutes, cleanupIntervalMinutes, TimeUnit.MINUTES);

        return cleanupTask;


    }

    private ExecutorService createCacheUpdaterTask() {
        ScheduledExecutorService cacheUpdater = Executors.newScheduledThreadPool(1);
        cacheUpdater.scheduleWithFixedDelay(new Runnable() {
            @Override
            public void run() {
                long start = System.currentTimeMillis();
                try {
                    MessageCache<BackplaneMessage> cache = daoFactory.getMessageCache();
                    long cacheMaxBytes = getMaxMessageCacheBytes();
                    if (cacheMaxBytes <= 0) return;
                    cache.setMaxCacheSizeBytes(cacheMaxBytes);
                    BackplaneMessage lastCached = cache.getLastMessage();
                    String lastCachedId = lastCached != null ? lastCached.getIdValue() : "";
                    cache.add(daoFactory.getBackplaneMessageDAO().retrieveMessagesNoScope(lastCachedId));
                } catch (Exception e) {
                    logger.error("Error updating message cache: " + e.getMessage(), e);
                }
                logger.info("Cache updated in " + (System.currentTimeMillis() - start) + " ms");
            }
        }, CACHE_UPDATER_INTERVAL_MILLISECONDS, CACHE_UPDATER_INTERVAL_MILLISECONDS, TimeUnit.MILLISECONDS);
        return cacheUpdater;
    }

    @PostConstruct
    private void init() {
        this.cleanup = createCleanupTask();
        this.cacheUpdater = createCacheUpdaterTask();

        for(SimpleDBTables table : EnumSet.allOf(SimpleDBTables.class)) {
            superSimpleDb.checkDomain(getTableName(table));
        }
    }

    @PreDestroy
    private void cleanup() {
        shutdownExecutor(cleanup);
        shutdownExecutor(cacheUpdater);
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

    @Inject
    @SuppressWarnings({"UnusedDeclaration"})
    private SuperSimpleDB superSimpleDb;

    @Inject
    private DaoFactory daoFactory;

    private Pair<BpServerConfigMap,Long> bpServerConfigCache;

    private String cachedGet(BpServerProperty property) throws SimpleDBException {
        Pair<BpServerConfigMap,Long> result = bpServerConfigCache;
        Long maxCacheAge = getMaxCacheAge();
        if (result == null || result.left == null || result.right == null || maxCacheAge == null ||
            result.right + maxCacheAge < System.currentTimeMillis() ) {
            synchronized (this) {
                result = bpServerConfigCache;
                if (result == null || result.left == null || result.right == null ||  maxCacheAge == null ||
                    result.right + maxCacheAge < System.currentTimeMillis() ) {
                    result = new Pair<BpServerConfigMap, Long>(superSimpleDb.retrieve(getBpServerConfigTableName(), BpServerConfigMap.class, BP_CONFIG_ENTRY_NAME), System.currentTimeMillis());
                    bpServerConfigCache = result;
                }
            }
        }
        return result.left == null ? null : result.left.get(property.name());
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
        return bpServerConfigCache != null && bpServerConfigCache.left != null ?
            Long.valueOf(bpServerConfigCache.left.get(BpServerProperty.CONFIG_CACHE_AGE_SECONDS.name())) :
            null;
    }

    public void checkAuth(String authTable, String user, String password) throws AuthException {
        try {
            User userEntry = superSimpleDb.retrieve(authTable, User.class, user);
            String authKey = userEntry == null ? null : userEntry.get(User.Field.PWDHASH);
            if ( ! HmacHashUtils.checkHmacHash(password, authKey) ) {
                throw new AuthException("User " + user + " not authorized in " + authTable);
            }
        } catch (SimpleDBException e) {
            throw new AuthException("User " + user + " not authorized in " + authTable + " , " + e.getMessage(), e);
        }
    }
    public static class BpServerConfigMap extends AbstractNamedMap {

        @SuppressWarnings({"UnusedDeclaration"}) // instantiation through reflection
        public BpServerConfigMap() { }

        @Override
        public void setName(String name) { }

        @Override
        public String getName() { return BP_CONFIG_ENTRY_NAME; }
    }

    private static class Pair<L,R> {
        public Pair(L left, R right) {
            this.left = left;
            this.right = right;
        }

        public final L left;
        public final R right;
    }
}
