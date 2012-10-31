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

package com.janrain.backplane.server;

import com.janrain.backplane.server.dao.redis.RedisBackplaneMessageDAO;
import com.janrain.backplane.server.dao.DaoFactory;
import com.janrain.redis.Redis;
import com.janrain.utils.BackplaneSystemProps;
import com.netflix.curator.framework.CuratorFramework;
import com.netflix.curator.framework.recipes.leader.LeaderSelectorListener;
import com.netflix.curator.framework.state.ConnectionState;
import com.yammer.metrics.Metrics;
import com.yammer.metrics.core.Histogram;
import com.yammer.metrics.core.MetricName;
import org.apache.log4j.Logger;
import redis.clients.jedis.Jedis;

import java.util.*;

/**
 * @author Tom Raney
 */
public class MessageProcessor implements LeaderSelectorListener {

	public MessageProcessor() {}

        /**
     * Processor to remove expired messages
     */
    public void cleanupMessages() {
        try {
            DaoFactory.getBackplaneMessageDAO().deleteExpiredMessages();
        } catch (Exception e) {
            logger.error(e);
        }
    }

    /**
     * Processor to pull messages off queue and make them available
     *
     */
    public void insertMessages() {
    	keepProcessingFlag = true;
    	logger.info("v1 message processor started");

    	Jedis jedis = Redis.getInstance().getWriteJedis();
    	logger.debug("retrieved jedis connection: " + jedis.toString());
    	do {

    		logger.debug("beginning message processor loop");

    		// set watch on the messages sorted set of keys
    		jedis.watch(RedisBackplaneMessageDAO.V1_MESSAGES);
    		List<byte[]> blPoppedResult = jedis.blpop(0, RedisBackplaneMessageDAO.V1_MESSAGE_QUEUE.getBytes());
    		logger.debug("blpop returned a message");
    		byte[] newMessageBytes = blPoppedResult.get(1);
    		jedis.lpush(V1_JOB_QUEUE, newMessageBytes);
    		MessageProcessingJob job = JobCreator.createJob(jedis, newMessageBytes);
	    	if (job.process()) {
	    		timeInQueue.update(job.timeInQueue());
	    	} else {
	    		handleRetry(jedis, newMessageBytes);
	    	}
	    	// TODO what happens if something above really fails and leaves the message on the job queue?
	    	// we need something to run periodically and look for messages older than some delta
	    	// then decide how we handle them
			jedis.lpop(V1_JOB_QUEUE);

    		jedis.unwatch();
    		
    	} while (keepProcessingFlag);

     }

	private void handleRetry(Jedis jedis, byte[] newMessageBytes) {
		int retries = getRetryCountForMessage(jedis, newMessageBytes);
		if (retries < MAX_RETRIES) {
			retries++;
			jedis.set(newMessageBytes, String.valueOf(retries).getBytes());
			jedis.rpush(RedisBackplaneMessageDAO.V1_MESSAGE_QUEUE.getBytes(), newMessageBytes);	    		
		} else {
			jedis.rpush(V1_FAILED_JOBS, newMessageBytes);
		}
	}

	private int getRetryCountForMessage(Jedis jedis, byte[] newMessageBytes) {
		byte[] retryCount = jedis.get(newMessageBytes);
		if (retryCount != null) { // we've retried this message before
			return Integer.valueOf(new String(retryCount));
		} else {
			return 0;
		}
	}

    private static final Logger logger = Logger.getLogger(MessageProcessor.class);

    public static final byte[] V1_JOB_QUEUE = "V1_JOB_QUEUE".getBytes();

    private static final int MAX_RETRIES = 3;

	private static final byte[] V1_FAILED_JOBS = "V1_FAILED_JOBS".getBytes();

   private final Histogram timeInQueue = Metrics.newHistogram(new MetricName("v1", this.getClass().getName().replace(".","_"), "time_in_queue"));

	private volatile boolean keepProcessingFlag;

    @Override
    public void takeLeadership(CuratorFramework curatorFramework) throws Exception {
        logger.info("[" + BackplaneSystemProps.getMachineName() + "] v1 leader elected for message processing");
        insertMessages();
        logger.info("[" + BackplaneSystemProps.getMachineName() + "] v1 leader ended message processing");
    }

    @Override
    public void stateChanged(CuratorFramework curatorFramework, ConnectionState connectionState) {
    	logger.info("state changed: "+connectionState);
    	switch (connectionState) {
    	case SUSPENDED:
    		keepProcessingFlag = false;			
    		break;
    	case LOST:
    		keepProcessingFlag = false;			
    		break;
    	default:
    		// if we quit processing, someone else should have been elected leader, we don't restart ourselves
    		break;
    	}
    }
}
