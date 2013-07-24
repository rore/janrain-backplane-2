package com.janrain.backplane.dao.redis

import com.janrain.backplane.common.model.{MessageField, BackplaneMessageBase}

/**
 * Mixin for RedisMessageDao with support for BackplaneMessage / Message Processor
 *
 * @author Johnny Bufu
 */
trait MessageProcessorDaoSupport[BMF <: MessageField,BMT <: BackplaneMessageBase[BMF]] {

  this: RedisMessageDao[BMT] =>

  val processorId: String = keyPrefix + "processor"

  val idField: BMF

  def busKey(bus: String): String = keyPrefix + "busIndex:" + bus

  def channelKey(channel: String): String = keyPrefix + "channelIndex:" + channel

  val messagesQueueKey: String = keyPrefix + "queue"

  val messagesKey: String = keyPrefix + "index"

  val lastIdKey: String = keyPrefix +  "lastId"

  // extend access scope from Redis/MessageDao'
  def itemKey(itemId: String): String = getKey(itemId)
  def mpInstantiate(data: Map[_,_]): BMT = instantiate(data)

}
