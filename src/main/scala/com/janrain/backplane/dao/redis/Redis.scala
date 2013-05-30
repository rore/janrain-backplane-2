package com.janrain.backplane.dao.redis

import com.redis.RedisClientPool
import com.janrain.util.{SystemProperties, Utils, Loggable}

/**
 * @author Johnny Bufu
 */
object Redis extends Loggable {
  private final val REDIS_DEFAULT_PORT = 6379
  private final val REDIS_DEFAULT_DB = 0
  private final val REDIS_CLIENTS_DEFAULT_MAX_IDLE = 30

  val (host, port, maxIdleConnections, redisDb) = (
    Utils.getRequiredSystemProperty(SystemProperties.REDIS_HOST),
    REDIS_DEFAULT_PORT,
    REDIS_CLIENTS_DEFAULT_MAX_IDLE,
    try {
      Utils.getOptionalSystemProperty(SystemProperties.REDIS_DB).map(_.toInt).getOrElse(REDIS_DEFAULT_DB)
    } catch {
      case e: Exception => REDIS_DEFAULT_DB
    })

  val isDefaultDb = redisDb == REDIS_DEFAULT_DB

  val pool = new RedisClientPool(host, port, maxIdleConnections, redisDb)
  logger.info("initialized redis pool for %s:%s (database# %s, max idle connections# %s)".format(host, port, redisDb, maxIdleConnections))
}
