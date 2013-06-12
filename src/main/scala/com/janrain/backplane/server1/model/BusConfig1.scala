package com.janrain.backplane.server1.model

import com.janrain.backplane.common.model.{MessageField, MessageFieldEnum, Message}
import com.janrain.backplane.common.MessageException

/**
 * @author Johnny Bufu
 */
class BusConfig1(data: Map[String,String]) extends Message(data, BusConfig1Fields.values) {

  def idField = BusConfig1Fields.BUS_NAME

  def isAllowedPost(user: String) = get(BusConfig1Fields.POST_USERS).exists(_.split(",").contains(user))

  def isAllowedGetAll(user: String) = get(BusConfig1Fields.GETALL_USERS).exists(_.split(",").contains(user))

}

object BusConfig1 {
  private[model] final val RETENTION_MIN_SECONDS = 60L
  private[model] final val RETENTION_MAX_SECONDS = 604800L        // one week
  private[model] final val RETENTION_STICKY_MIN_SECONDS = 28800L  // eight hours
  private[model] final val RETENTION_STICKY_MAX_SECONDS = 604800L // one week
}

object BusConfig1Fields extends MessageFieldEnum {

  type BusConfig1Field = EnumVal

  sealed trait EnumVal extends Value with MessageField {
    def required = true
  }

  val BUS_NAME = new BusConfig1Field { def name = "bus_name" }

  val RETENTION_TIME_SECONDS = new BusConfig1Field { def name = "retention_time_seconds"

    override def validate(fieldValue: Option[String], wholeMessage: Message[_]) {
      super.validate(fieldValue, wholeMessage)
      validateLong(fieldValue)
      fieldValue.map(_.toLong).foreach(longValue =>
        if (longValue < BusConfig1.RETENTION_MIN_SECONDS || longValue > BusConfig1.RETENTION_MAX_SECONDS)
          throw new MessageException("Value of " + name + " = " + longValue + " but must be between " + BusConfig1.RETENTION_MIN_SECONDS + " and " + BusConfig1.RETENTION_MAX_SECONDS)
      )
    }
  }

  val RETENTION_STICKY_TIME_SECONDS = new BusConfig1Field { def name = "retention_sticky_time_seconds"

    override def validate(fieldValue: Option[String], wholeMessage: Message[_]) {
      super.validate(fieldValue, wholeMessage)
      validateLong(fieldValue)
      fieldValue.map(_.toLong).foreach(longValue =>
        if (longValue < BusConfig1.RETENTION_STICKY_MIN_SECONDS || longValue > BusConfig1.RETENTION_STICKY_MAX_SECONDS)
          throw new MessageException("Value of " + name + " = " + longValue + " but must be between " + BusConfig1.RETENTION_MIN_SECONDS + " and " + BusConfig1.RETENTION_STICKY_MAX_SECONDS)
      )
    }
  }

  val POST_USERS = new BusConfig1Field { def name = "post_users" }

  val GETALL_USERS = new BusConfig1Field { def name = "getall_users" }
}
