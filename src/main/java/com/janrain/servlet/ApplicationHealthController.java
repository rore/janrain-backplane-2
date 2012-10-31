package com.janrain.servlet;

import java.util.HashMap;
import java.util.Map;

import javax.inject.Inject;

import org.springframework.stereotype.Controller;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.ResponseBody;

import redis.clients.jedis.Jedis;
import redis.clients.jedis.exceptions.JedisException;

import com.janrain.backplane.server.config.Backplane1Config;
import com.janrain.redis.Redis;


@Controller
@RequestMapping(value="/health*")
public class ApplicationHealthController {

	@Inject
    private Backplane1Config bpConfig;
	

	@RequestMapping
	public @ResponseBody ApplicationHealthResponse check() {
		ApplicationHealthResponse response = new ApplicationHealthResponse(bpConfig);
		Redis redisInstance = Redis.getInstance();
		Jedis readJedis = redisInstance.getReadJedis();
		Jedis writeJedis = redisInstance.getWriteJedis();
		try {
			response.setReadPingResponse(readJedis.ping());
			response.setWritePingResponse(writeJedis.ping());
			response.setWriteRedisInfo(mapRedisInfo(writeJedis.info()));
			response.setReadRedisInfo(mapRedisInfo(readJedis.info()));
		} catch (JedisException je) {
			response.setSystemStatus("error:"+ je.getMessage());
		} finally {
			redisInstance.releaseToPool(readJedis);
			redisInstance.releaseToPool(writeJedis);
		}
		response.setSystemStatus("OK");
		return response;
	}

	private Map<String, String> mapRedisInfo(String info) {
        String[] lines = info.split("\\r?\\n");
        Map<String, String> items = new HashMap<String, String>(lines.length +1);
        for (String line : lines) {
                String[] entry = line.split(":");
                items.put(entry[0], entry[1]);
        }	
        return items;
	}
}
