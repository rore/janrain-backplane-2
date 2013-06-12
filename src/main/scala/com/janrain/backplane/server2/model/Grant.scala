package com.janrain.backplane.server2.model

import com.janrain.backplane.common.model.{Message, MessageField, MessageFieldEnum}
import com.janrain.backplane2.server.{GrantState, Scope, GrantType}
import com.janrain.backplane.common.{DateTimeUtils, MessageException}
import com.janrain.oauth2.TokenException
import org.apache.commons.lang.StringUtils

/**
 * @author Johnny Bufu
 */
class Grant(data: Map[String,String]) extends Message(data, GrantFields.values) {
  def idField = GrantFields.ID
}

object Grant {
  private[model] final val CODE_ID_LENGTH = 20L
  private[model] final val CODE_EXPIRATION_SECONDS_DEFAULT = 600L
}

object GrantFields extends MessageFieldEnum {

  type GrantField = EnumVal

  sealed trait EnumVal extends Value with MessageField {
    def required = true
  }

  val ID = new GrantField { def name = "id" }

  val TYPE = new GrantField { def name = "type"

    override def validate(fieldValue: Option[String], wholeMessage: Message[_]) {
      super.validate(fieldValue, wholeMessage)
      fieldValue.foreach(grantValue =>
        try {
          GrantType.valueOf(grantValue)
        } catch {
          case e: Throwable => throw new MessageException("Invalid grant type: " + grantValue)
        })
    }
  }

  val ISSUED_BY_USER_ID = new GrantField { def name = "issued_by_user_id" }

  val ISSUED_TO_CLIENT_ID = new GrantField { def name = "issued_to_client_id" }

  val AUTHORIZED_SCOPES = new GrantField { def name = "authorized_scopes"

    override def validate(fieldValue: Option[String], wholeMessage: Message[_]) {
      super.validate(fieldValue, wholeMessage)
      fieldValue.foreach(scopeValue =>
        try {
          new Scope(scopeValue)
        } catch {
          case e: TokenException => throw new MessageException("Invalid grant scope: " + scopeValue)
        })
    }
  }

  val STATE = new GrantField { def name = "state"

    override def validate(fieldValue: Option[String], wholeMessage: Message[_]) {
      super.validate(fieldValue, wholeMessage)
      fieldValue.foreach(stateValue =>
        try {
          GrantState.valueOf(stateValue)
        } catch {
          case e: Throwable => throw new MessageException("Invalid grant value for state: " + stateValue)
        })
    }
  }

  val TIME_UPDATE = new GrantField { def name = "time_update"

    override def validate(fieldValue: Option[String], wholeMessage: Message[_]) {
      super.validate(fieldValue, wholeMessage)
      fieldValue.foreach(timeValue =>
        try {
          DateTimeUtils.ISO8601.get().parse(timeValue)
        } catch {
          case e: Throwable => throw new MessageException("Invalid grant value for time_update: " + timeValue)
        })
    }
  }

  val TIME_EXPIRE = new GrantField { def name = "time_expire"

    override def validate(fieldValue: Option[String], wholeMessage: Message[_]) {
      super.validate(fieldValue, wholeMessage)
      fieldValue.foreach(timeValue =>
        try {
          if (StringUtils.isNotEmpty(timeValue)) {
            DateTimeUtils.ISO8601.get().parse(timeValue)
          }
        } catch {
          case e: Throwable => throw new MessageException("Invalid grant value for time_expire: " + timeValue)
        })
    }
  }

}
