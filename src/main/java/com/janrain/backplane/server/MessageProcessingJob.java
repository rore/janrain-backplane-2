package com.janrain.backplane.server;

import java.text.ParseException;
import java.util.Date;
import java.util.Set;

import org.apache.commons.lang.SerializationUtils;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;

import com.janrain.backplane.server.config.Backplane1Config;
import com.janrain.backplane.server.dao.DaoFactory;
import com.janrain.backplane.server.dao.redis.RedisBackplaneMessageDAO;
import com.janrain.commons.util.Pair;

import redis.clients.jedis.Jedis;
import redis.clients.jedis.Transaction;

public class MessageProcessingJob {

	private static final Logger logger = Logger
			.getLogger(com.janrain.backplane.server.MessageProcessingJob.class);
	private Jedis jedis;
	private BackplaneMessage message;
	private Pair<String, Date> lastIdAndDate;

	public MessageProcessingJob(Jedis jedis, BackplaneMessage backplaneMessage) {
		this.jedis = jedis;
		this.message = backplaneMessage;
	}

	public boolean process() {
		if (!setup()) {
			return false;
		}
		Transaction transaction = jedis.multi();

		// retrieve the expiration config per the bus
		BusConfig1 busConfig1 = DaoFactory.getBusDAO().get(message.getBus());
		int retentionTimeSeconds = 60;
		int retentionTimeStickySeconds = 3600;
		if (busConfig1 != null) {
			// should be here in normal flow
			retentionTimeSeconds = busConfig1.getRetentionTimeSeconds();
			retentionTimeStickySeconds = busConfig1
					.getRetentionTimeStickySeconds();
		}

		String oldId = message.getIdValue();
		// TOTAL ORDER GUARANTEE
		// verify that the date portion of the new message ID is greater than
		// all existing message ID dates
		// if not, uptick id by 1 ms and insert
		// this means that all message ids have unique time stamps, even if they
		// arrived at the same time.

		lastIdAndDate = message.updateId(lastIdAndDate);
		String newId = message.getIdValue();

		// messageTime is guaranteed to be a unique identifier of the message
		// because of the TOTAL ORDER mechanism above
		long messageTime = BackplaneMessage.getDateFromId(newId).getTime();

		// <ATOMIC>
		// save the individual message by key
		transaction.set(RedisBackplaneMessageDAO.getKey(newId),
				SerializationUtils.serialize(message));
		// set the message TTL
		if (message.isSticky()) {
			transaction.expire(RedisBackplaneMessageDAO.getKey(newId),
					retentionTimeStickySeconds);
		} else {
			transaction.expire(RedisBackplaneMessageDAO.getKey(newId),
					retentionTimeSeconds);
		}

		// add message id to channel list
		transaction.rpush(
				RedisBackplaneMessageDAO.getChannelKey(message.getChannel()),
				newId.getBytes());

		// add message id to sorted set of all message ids as an index
		String metaData = message.getBus() + " " + message.getChannel() + " "
				+ newId;
		transaction.zadd(RedisBackplaneMessageDAO.V1_MESSAGES.getBytes(),
				messageTime, metaData.getBytes());

		// add message id to sorted set keyed by bus as an index
		transaction.zadd(RedisBackplaneMessageDAO.getBusKey(message.getBus()),
				messageTime, newId.getBytes());

		// make sure all subscribers get the update
		transaction.publish("alerts", newId);

		// pop one message off the queue - which will only happen if this
		// transaction is successful
		transaction.lpop(RedisBackplaneMessageDAO.V1_MESSAGE_QUEUE);
		// </ATOMIC>

		logger.info("pipelined message " + oldId + " -> " + newId);

		if (transaction.exec() == null) {
            	// requeue message or discard? 
			return false;
		}
		return true;

	}

	public boolean setup() {
		try {
			lastIdAndDate = retrieveLatestLiveMessageId(jedis);
			return true;
		} catch (ParseException e) {
			logger.error("Unable to set up new message processing job", e);
		} 
		return false;
	}

	private Pair<String, Date> retrieveLatestLiveMessageId(Jedis jedis) throws ParseException {
		// retrieve the latest 'live' message ID
		String latestMessageId = null;
		Set<String> latestMessageMetaSet = jedis.zrange(RedisBackplaneMessageDAO.V1_MESSAGES, -1, -1);

		if (latestMessageMetaSet != null && !latestMessageMetaSet.isEmpty()) {
		    String[] segs = latestMessageMetaSet.iterator().next().split(" ");
		    if (segs.length == 3) {
		        latestMessageId = segs[2];
		    }
		}

		Pair<String, Date> lastIdAndDate =  new Pair<String, Date>("", new Date(0));
	    lastIdAndDate = StringUtils.isEmpty(latestMessageId) ?
	            new Pair<String, Date>("", new Date(0)) :
	            new Pair<String, Date>(latestMessageId, Backplane1Config.ISO8601.get().parse(latestMessageId.substring(0, latestMessageId.indexOf("Z") + 1)));
		return lastIdAndDate;
	}

	public void teardown() {
		// room to grow
	}

	public long timeInQueue() {
		long now = System.currentTimeMillis();
		long diff = now - BackplaneMessage.getDateFromId(message.getIdValue()).getTime();
		if (diff < 0 || diff > 2880000) {
			logger.warn("time diff is bizarre at: " + diff);
		}
		return diff;
	}
}
