package com.janrain.backplane.server2.model

import com.janrain.backplane.common.model.{Message, UserFieldEnum, User}
import com.janrain.oauth2.{ValidationException, OAuth2}
import com.janrain.servlet.InvalidRequestException

/**
 * @author Johnny Bufu
 */
class Client(data: Map[String,String]) extends User("bp2Client", data, ClientFields.values) {

  def this(username: String, pwdhash: String) = this(Map(
    ClientFields.USER.name -> username,
    ClientFields.PWDHASH.name -> pwdhash
  ))

  def idField = ClientFields.USER
}

object ClientFields extends UserFieldEnum {

  trait ClientEnumVal extends EnumVal {
    def required = true
  }

  val SOURCE_URL = new ClientEnumVal { def name = "source_url" }

  val REDIRECT_URI = new ClientEnumVal { def name = "redirect_uri"
    override def validate(fieldValue: Option[String], wholeMessage: Message[_]) {
      super.validate(fieldValue, wholeMessage)
      try {
        fieldValue.foreach(OAuth2.validateRedirectUri(_))
      } catch {
        case e: ValidationException => throw new InvalidRequestException(e.getMessage)
      }
    }
  }
}
