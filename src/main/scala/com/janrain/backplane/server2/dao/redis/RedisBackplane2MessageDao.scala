package com.janrain.backplane.server2.dao.redis

import com.janrain.backplane.server2.dao.Backplane2MessageDao
import com.janrain.backplane.dao.redis.{MessageProcessorDaoSupport, Redis, RedisMessageDao}
import com.janrain.backplane.server2.model.{Backplane2MessageFields, Backplane2Message}
import com.janrain.backplane2.server.Scope
import com.redis.RedisClient
import com.janrain.backplane.common.model.{BackplaneMessage, Message}
import scala.collection.JavaConversions._
import com.janrain.util.RandomUtils

/**
 * @author Johnny Bufu
 */
object RedisBackplane2MessageDao extends RedisMessageDao[Backplane2Message]("bp2Message:")
  with Backplane2MessageDao
  with MessageProcessorDaoSupport[Backplane2MessageFields.EnumVal,Backplane2Message] {

  private final val MAX_MSGS_IN_FRAME = 25

  private[redis] final val INDEXED_SCOPE_FIELDS = Map(
    Backplane2MessageFields.CHANNEL -> channelKey _,
    Backplane2MessageFields.BUS -> busKey _ )

  val idField = Backplane2MessageFields.ID

  protected def instantiate(data: Map[_, _]) = new Backplane2Message(data.map(kv => kv._1.toString -> kv._2.toString))

  override protected[redis] def getKey(itemId: String) = super.getKey(itemId)

  override def store(item: Backplane2Message) {
    Redis.writePool.withClient(_.rpush(messagesQueueKey, item.serialize))
  }

  override def messageCount(channel: String): Long = Redis.readPool.withClient(_.zcard(channelKey(channel))).getOrElse(0)

  def retrieveMessagesPerScope(scope: Scope, since: String): (List[Backplane2Message], Boolean, Option[String]) = {
    val pipelineResponse = Redis.readPool.withClient(_.pipeline( p => {
      p.zrange(messagesKey, -1, -1, RedisClient.ASC) // extract last msg metadata /ID
      // logical OR for all indexed scope fields of the same type
      val unions = for {
        (indexedScope, indexKey) <- INDEXED_SCOPE_FIELDS
        scopeValues = scope.getScopeFieldValues(indexedScope)
        if ! scopeValues.isEmpty
      } yield {
        val union = "scope_req_zunionstore:" + indexedScope + ":" + RandomUtils.randomString(10)
        scopeValues.foreach( scopeValue =>
          p.zunionstore(union, List(union, indexKey(scopeValue)), RedisClient.MAX)
        )
        union
      }
      // logical AND for all index scope fields of different type
      unions.foldLeft(Option.empty[String]) {
        case (None, union) => Some(union) // user first elem as the first intermediary result
        case (Some(intersectionResult), union) => {
          p.zinterstore(intersectionResult, List(intersectionResult, union), RedisClient.MAX)
          Some(intersectionResult)
        }
      }
      // filter by time/since
      .map(p.zrangebyscore(_, BackplaneMessage.timeFromId(since), minInclusive = false, Double.MaxValue, maxInclusive = true, None, RedisClient.ASC))

      unions.foreach(p.del(_))
    }))

    val lastAvailableMsgId = pipelineResponse.map {
      case List(Some(List(lastMsgMetadata: String)), _) => lastMsgMetadata.split(" ")
      case _ => Array.empty
    }.collect {
      case Array(bus, channel, msgId, expTime) => msgId
    }

    val messages = pipelineResponse.map(_.collect { // the zrangebyscore Option[List] result
      case Some(msgIds: List[_]) if !msgIds.isEmpty => {
        Redis.readPool.withClient(_.mget(msgIds.head, msgIds.tail: _*)).map(_.flatten)
        .map(_.map(ser => instantiate(Message.deserialize(ser))))
      }
    }).map(_.flatten.flatten)

    messages.map(m => {
      val filtered = m.filter(scope.isMessageInScope).take(MAX_MSGS_IN_FRAME)
      (filtered, filtered.size != m.size, lastAvailableMsgId)
    }).getOrElse(Nil, false, lastAvailableMsgId)
  }
}
