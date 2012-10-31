package com.janrain.backplane.server;

import org.apache.commons.lang.SerializationUtils;


import redis.clients.jedis.Jedis;

public class JobCreator {

	public static MessageProcessingJob createJob(Jedis jedis, byte[] messageToProcess) {
		BackplaneMessage backplaneMessage = (BackplaneMessage) SerializationUtils.deserialize(messageToProcess);
		return new MessageProcessingJob(jedis, backplaneMessage);
	}

}
