package com.janrain.backplane.server2.dao

import com.janrain.backplane.dao.{MessageDao, DAO, DaoAll}
import com.janrain.commons.supersimpledb.message.NamedMap
import scala.collection.JavaConversions
import com.janrain.backplane.common.model.Message

/**
 * Stackable modification trait for Message DAOs
 * that transparently forwards (read) DAO operations to the legacy DAO object/layer.
 *
 * Store operation(s) can be configured at the model layer and routed to either legacy or new DAO layer.
 *
 * @author Johnny Bufu
 */
trait LegacyDaoForwarder[LT <: NamedMap, T <: Message[_] with LegacySupport[LT]] extends MessageDao[T] with DaoAll[T] {

  val legacyDao: DAO[LT]

  def preferLegacyGet(id: String): Boolean

  def isLegacyStore(item: T): Boolean

  abstract override def get(id: String): Option[T] = {
    if (preferLegacyGet(id)) {
      val legacyItem = legacyDao.get(id)
      if (legacyItem != null)
        Option(instantiate(JavaConversions.mapAsScalaMap(legacyItem).toMap))
      else
        super.get(id)
    } else {
      val nonLegacyItem = super.get(id)
      if (nonLegacyItem.isDefined)
        nonLegacyItem
      else
        Some(instantiate(JavaConversions.mapAsScalaMap(legacyDao.get(id)).toMap))
    }
  }

  abstract override def delete(id: String) = {
    legacyDao.delete(id)
    super.delete(id)
  }

  abstract override def getAll: List[T] = {
    val legacyItems = JavaConversions.asScalaBuffer(legacyDao.getAll)
      .map( legacyItem => instantiate(JavaConversions.mapAsScalaMap(legacyItem).toMap)).toList
    super.getAll ++ legacyItems
  }

  abstract override def store(item: T) {
    if (isLegacyStore(item)) legacyDao.persist(item.asLegacy)
    else super.store(item)
  }

}
