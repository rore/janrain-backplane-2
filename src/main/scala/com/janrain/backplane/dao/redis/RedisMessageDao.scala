package com.janrain.backplane.dao.redis

import com.janrain.backplane.Message
import com.janrain.backplane.dao.{DaoException, Dao}
import com.janrain.util.SystemProperties
import com.janrain.util.Loggable


/**
 * @author Johnny Bufu
 */
abstract class RedisMessageDao[MT <: Message[_]](val keyPrefix: String) extends Dao[MT] with Loggable {

  protected def getKey(itemId: String) = SystemProperties.INSTANCE_ID + ":" + keyPrefix + itemId

  protected def instantiate(data: Map[_,_]): MT

  // redis lib returns Some(Map()) instead of None, so:
  private def instantiateEmpty(data: Option[Map[_,_]]): Option[MT] = data match {
    case Some(map) if ! map.isEmpty => Some(instantiate(map))
    case _ => None
  }

  def expire(seconds: Int, id: String) {
    if (! Redis.pool.withClient(_.expire(getKey(id), seconds)) )
      throw new DaoException("expire failed for %s : redis returned false".format(getKey(id)))
  }

  def expire(seconds: Int, ids: String*): List[(String, Boolean)] =
    ids.zip(
      Redis.pool.withClient( _.pipeline( p => {
        for (id <- ids)
          p.expire(getKey(id), seconds)
      }))
      .getOrElse(throw new DaoException("multi-expire failed for key prefix %s  items: [%s]".format(keyPrefix, ids.mkString(","))))
      .map {
        case bRes: Boolean => bRes
        case err => throw new DaoException("multi-expire failed for key prefix %s  ids: [%s] -- unexpected result: %s"
          .format(keyPrefix, ids.mkString(","),err))
      }
    ).toList

  def store(item: MT) {
    if (! Redis.pool.withClient(_.hmset(getKey(item.id), item)))
      throw new DaoException("store failed for %s : redis returned false".format(getKey(item.id)))
  }

  def store(items: MT*): List[(String, Boolean)] =
    items.map(_.id).zip( // one pipeline response entry per pipelined request
      Redis.pool.withClient( _.pipeline {
        p => {
          for (item <- items)
            p.hmset(getKey(item.id), item)
        }
      })
      .getOrElse(throw new DaoException("multi-store failed for key prefix %s  items: [%s]".format(keyPrefix, items.map(_.id).mkString(","))))
      .map {
        case bRes: Boolean => bRes
        case err => throw new DaoException("multi-store failed for key prefix %s  items: [%s] -- unexpected result: %s"
                              .format(keyPrefix, items.map(_.id).mkString(","),err))
      }
    ).toList

  override def get(id: String): Option[MT] =
    instantiateEmpty(Redis.pool.withClient(_.hgetall(getKey(id))))

  def get(ids: String*): List[(String,Option[MT])] =
    ids.zip(
      Redis.pool.withClient(_.pipeline {
        p => ids.foreach(id => p.hgetall(getKey(id)))
      })
      .getOrElse(throw new DaoException("multi-get failed for key prefix %s  items: [%s]".format(keyPrefix, ids.mkString(","))))
      .map( _ match {
        case Some(m: Map[_,_]) => instantiateEmpty(Some(m))
        case err => throw new DaoException("multi-get failed for key prefix %s  items: [%s] -- unexpected result: %s"
                              .format(keyPrefix, ids.mkString(","), err))
      })
    ).toList

  // todo: update same as store, should throw if item does not exist in db?
  def update(item: MT) {
    store(item)
  }

  def update(items: MT*) = store(items: _*)

  def delete(id: String) = Redis.pool.withClient(_.del(getKey(id)))
    .getOrElse(throw new DaoException(("deleted failed for key %s".format("")))) == 1L

  def delete(ids: String*) =
    ids.zip(
      // redis del(id*) returns the total number of deletes, but we need to return whether each id was deleted or not
      Redis.pool.withClient( _.pipeline {
        p => ids.foreach(id => p.del(getKey(id)))
      })
      .getOrElse(throw new DaoException("error attempting to delete keys: [%s]".format(ids.mkString(", "))))
      .map {
        case Some(delRes: Long) => delRes == 1L
        case err => throw new DaoException("multi-deleted failed for key prefix %s  items: [%s] -- unexpected result: %s"
          .format(keyPrefix, ids.mkString(","),err))
      }
    ).toList

  def retrieveAndDelete(itemId: String) = {
    val itemKey = getKey(itemId)
    val pipelineResponse = Redis.pool.withClient(_.pipeline {
      p => {
        p.hgetall(itemKey)
        p.del(itemKey)
      }
    }).getOrElse(throw new DaoException("item id: %s not found for retrieve-and-delete".format(itemKey)))

    (pipelineResponse.contains(Some(1L)), pipelineResponse.head) match {

      case (true, Some(mapResponse: Map[_,_])) =>
        Some(instantiate(mapResponse))

      case (false, _) =>
        logger.warn("retieve-and-delete for item ID %s failed, delete result different than 1L".format(itemKey))
        None

      case _ =>
        None
    }
  }
}
