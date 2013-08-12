package com.janrain.backplane.server2.dao

import com.janrain.backplane.dao.{MessageDao, DAO, DaoAll}
import com.janrain.commons.supersimpledb.message.NamedMap
import scala.collection.JavaConversions
import com.janrain.backplane.common.model.Message
import com.janrain.util.Loggable

/**
 * Stackable modification trait for Message DAOs
 * that transparently forwards (read) DAO operations to the legacy DAO object/layer.
 *
 * Store operation(s) can be configured at the model layer and routed to either legacy or new DAO layer.
 *
 * @author Johnny Bufu
 */
trait LegacyDaoForwarder[LT <: NamedMap, T <: Message[_] with LegacySupport[LT]] extends MessageDao[T] with DaoAll[T] with Loggable {

  val legacyDao: DAO[LT]

  abstract override def get(id: String): Option[T] = super.get(id) match {
    case None => try {
      val legacyItem = legacyDao.get(id)
      val convertedItem = instantiate(JavaConversions.mapAsScalaMap(legacyItem).toMap)
      // should happen only once, instantiate/convert throws NPE if legacyDao returns null
      // then (after one successful super.store) new DAO (super.get) will find this item/id
      super.store(convertedItem)
      logDebug("converted %s : %s to new dao/format".format(legacyItem.getClass.getSimpleName, legacyItem.getName))
      Some(convertedItem)
    } catch {
      case npe: NullPointerException => None
    }

    case nonLegacyItem => nonLegacyItem
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
    legacyDao.persist(item.asLegacy)
    super.store(item)
  }

}
