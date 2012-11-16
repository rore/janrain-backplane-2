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

package com.janrain.backplane.config;

import com.janrain.backplane.common.AuthException;
import com.janrain.backplane.common.BackplaneServerException;
import com.janrain.backplane.server.MessageProcessor;
import com.janrain.backplane2.server.V2MessageProcessor;
import com.janrain.backplane2.server.config.User;
import com.janrain.backplane2.server.dao.DAOFactory;
import com.janrain.cache.CachedL1;
import com.janrain.commons.util.AwsUtility;
import com.janrain.commons.util.InitSystemProps;
import com.janrain.commons.util.Pair;
import com.janrain.crypto.HmacHashUtils;
import com.janrain.redis.Redis;
import com.netflix.curator.framework.CuratorFramework;
import com.netflix.curator.framework.CuratorFrameworkFactory;
import com.netflix.curator.framework.recipes.leader.LeaderSelector;
import com.netflix.curator.framework.recipes.leader.LeaderSelectorListener;
import com.netflix.curator.retry.ExponentialBackoffRetry;
import com.yammer.metrics.Metrics;
import com.yammer.metrics.reporting.ConsoleReporter;
import com.yammer.metrics.reporting.GraphiteReporter;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.springframework.context.annotation.Scope;

import javax.annotation.PostConstruct;
import javax.annotation.PreDestroy;
import javax.inject.Inject;
import java.util.HashMap;
import java.util.Map;
import java.util.Properties;
import java.util.concurrent.ExecutorService;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

import static com.janrain.backplane.config.BackplaneSystemProps.ZOOKEEPER_SERVERS;


/**
 * Holds configuration settings for the Backplane server
 * 
 * @author Jason Cowley, Johnny Bufu
 */
@Scope(value="singleton")
public class BackplaneConfig {

    // - PUBLIC

    /**
	 * @return the debugMode
	 */
	public static boolean isDebugMode() {
        return Boolean.valueOf(cachedGet(BpServerConfig.Field.DEBUG_MODE));
	}

    /**
     * @return the server default max message value per channel
     * @throws SimpleDBException
     */
    public static long getDefaultMaxMessageLimit() {
        Long max = Long.valueOf(cachedGet(BpServerConfig.Field.DEFAULT_MESSAGES_MAX));
        return max == null ? BackplaneConfig.BP_MAX_MESSAGES_DEFAULT : max;
    }

    public static boolean isLeaderDisabled() {
        // skip DAO layer, not so crazy about editing serialized streams for debug, yay FED-76
        return isDebugMode() && Redis.getInstance().get(EC2InstanceId) != null;
    }

    public static Exception getDebugException(Exception e) {
        return isDebugMode() ? e: null;
     }

    /**
     * Retrieve the server instance id Amazon assigned
     * @return
     */
    public static String getEC2InstanceId() {
        return EC2InstanceId;
    }

    public static void addToBackgroundServices(String workerName, ScheduledExecutorService messageWorkerTask) {
        backgroundServices.put(workerName, messageWorkerTask);
    }

    public String getInstanceId() {
        return bpInstanceId;
    }

    public String getBuildVersion() {
        return buildProperties.getProperty(BUILD_VERSION_PROPERTY);
    }

    public void checkAdminAuth(String user, String password) throws AuthException {
        try {
            User userEntry = daoFactory.getAdminDAO().get(user);
            String authKey = userEntry == null ? null : userEntry.get(User.Field.PWDHASH);
            if ( ! HmacHashUtils.checkHmacHash(password, authKey) ) {
                logger.error("User " + user + " not authorized");
                throw new AuthException("Access denied");
            }
        } catch (BackplaneServerException e) {
            logger.error("Error authenticating user " + user + " : " + e.getMessage(), getDebugException(e));
            throw new AuthException("User " + user + " not authorized, " + e.getMessage(), getDebugException(e));
        }
    }

    // - PACKAGE

    BackplaneConfig(String instanceId) {
        this.bpInstanceId = instanceId;
    }

    // - PRIVATE

    private static final Logger logger = Logger.getLogger(BackplaneConfig.class);

    private static final String BUILD_PROPERTIES = "/build.properties";
    private static final String BUILD_VERSION_PROPERTY = "build.version";
    private static final Properties buildProperties = new Properties();
    private static final long BP_MAX_MESSAGES_DEFAULT = 100;

    private final String bpInstanceId;

    private static final Map<String, ExecutorService> backgroundServices = new HashMap<String, ExecutorService>();

    // Amazon specific instance-id value
    private static String EC2InstanceId = AwsUtility.retrieveEC2InstanceId();

    @SuppressWarnings({"UnusedDeclaration"})
    private BackplaneConfig() {
        bpInstanceId = getAwsProp(InitSystemProps.AWS_INSTANCE_ID);

        ConsoleReporter.enable(10, TimeUnit.MINUTES);

        // Dump metrics to graphite server
        String graphiteServer = System.getProperty(BackplaneSystemProps.GRAPHITE_SERVER);
        if (StringUtils.isNotBlank(graphiteServer)) {
            try {
                String args[] = graphiteServer.split(":");
                String server = args[0];
                int port = Integer.parseInt(args[1]);
                GraphiteReporter.enable(10, TimeUnit.SECONDS, server, port, BackplaneSystemProps.getMachineName().replace(".","_") + "_" + bpInstanceId);
                logger.info("Graphite server enabled at " + graphiteServer);
            } catch (Exception e) {
                logger.warn("could not enable Graphite from " + graphiteServer + " must be in the form SERVER:PORT");
            }
        }
        try {
            buildProperties.load(BackplaneConfig.class.getResourceAsStream(BUILD_PROPERTIES));
            //assert(StringUtils.isNotBlank(getEncryptionKey()));
        } catch (Exception e) {
            String err = "Error loading build properties from " + BUILD_PROPERTIES;
            logger.error(err, e);
            throw new RuntimeException(err, e);
        }

        logger.info("Configured Backplane Server instance: " + bpInstanceId);
    }

    private Pair<String, ExecutorService> createPingTask() {
        String label = "redis ping";
        ScheduledExecutorService ping = Executors.newScheduledThreadPool(1);
        ping.scheduleWithFixedDelay(new Runnable() {
            @Override
            public void run() {
                com.janrain.redis.Redis.getInstance().ping();
            }
        }, 30, 10, TimeUnit.SECONDS);
        return new Pair<String, ExecutorService>(label, ping);
    }

    private void addTask(Map<String, ExecutorService> backgroundServices, Pair<String, ExecutorService> nameAndService) {
        backgroundServices.put(nameAndService.getLeft(), nameAndService.getRight());
    }

    @PostConstruct
    private void init() {
        addTask(backgroundServices, createPingTask());
        initZk("/v1_worker", new MessageProcessor());
        initZk("/v2_worker", new V2MessageProcessor(daoFactory));
    }

    private void initZk(String leaderPath, LeaderSelectorListener listener) {
        try {
            String zkServerConfig = System.getProperty(ZOOKEEPER_SERVERS);
            if (StringUtils.isEmpty(zkServerConfig)) {
                logger.error("Cannot find configuration entry for ZooKeeper server ('" + ZOOKEEPER_SERVERS + "' system property)" );
                System.exit(1);
            }
            CuratorFramework client = CuratorFrameworkFactory.newClient(zkServerConfig, new ExponentialBackoffRetry(50, 20));
            client.start();
            LeaderSelector leaderSelector = new LeaderSelector(client, leaderPath, listener);
            leaderSelector.autoRequeue();
            leaderSelector.start();
            com.janrain.redis.Redis.getInstance().setActiveRedisInstance(client);
        } catch (Exception e) {
            logger.error(e);
        }
    }

    @PreDestroy
    private void cleanup() {
        Metrics.shutdown();
        for (Map.Entry<String, ExecutorService> serviceEntry : backgroundServices.entrySet()) {
            shutdownExecutor(serviceEntry.getKey(), serviceEntry.getValue());
        }
    }

    private void shutdownExecutor(String serviceName, ExecutorService executor) {
        try {
            executor.shutdown();
            if (executor.awaitTermination(10, TimeUnit.SECONDS)) {
                logger.info(serviceName + " background thread shutdown properly");
            } else {
                executor.shutdownNow();
                if (!executor.awaitTermination(10, TimeUnit.SECONDS)) {
                    logger.error(serviceName + " background thread did not terminate");
                }
            }
        } catch (InterruptedException e) {
            logger.error(serviceName + " termination threw an exception", e);
            executor.shutdownNow();
            Thread.currentThread().interrupt();
        }
    }

    @Inject
    private DAOFactory daoFactory;

    /**
     * Load system property
     * @param propParamName
     * @return
     */
    private static String getAwsProp(String propParamName) {
        String result = System.getProperty(propParamName);
        if (StringUtils.isBlank(result)) {
            throw new RuntimeException("Required system property configuration missing: " + propParamName);
        }
        return result;
    }

    private static String cachedGet(BpServerConfig.Field property) {
        try {
            BpServerConfig bpServerConfigCache = (BpServerConfig) CachedL1.getInstance().getObject(BackplaneSystemProps.BPSERVER_CONFIG_KEY);
            if (bpServerConfigCache == null) {
                // pull from db if not found in cache
                try {
                    bpServerConfigCache = daoFactory.getConfigDAO().get(BackplaneSystemProps.BPSERVER_CONFIG_KEY);
                } catch (Exception e) {
                    // if we get an error from the db, create a new object
                }

                if (bpServerConfigCache == null) {
                    // no instance found in cache or the db, so let's use the default record
                    bpServerConfigCache = new BpServerConfig();
                }
                // add it to the L1 cache
                CachedL1.getInstance().setObject(BackplaneSystemProps.BPSERVER_CONFIG_KEY, -1, bpServerConfigCache);
            }

            return bpServerConfigCache.get(property);

        } catch (Exception e) {
            logger.error(e);
            return null;
        }
    }
}
