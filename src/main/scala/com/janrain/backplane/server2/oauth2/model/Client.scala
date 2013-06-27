package com.janrain.backplane.server2.oauth2.model

import com.janrain.backplane.common.model.{Message, UserFieldEnum, User}
import com.janrain.oauth2.{ValidationException, OAuth2}
import com.janrain.servlet.InvalidRequestException
import com.janrain.backplane.config.model.Password
import scala.collection.JavaConversions._

/**
 * @author Johnny Bufu
 */
class Client(data: Map[String,String]) extends User("bp2Client", data, ClientFields.values)
  with Password[ClientFields.EnumVal, Client] {

  def this(javaData: java.util.Map[String,String]) = this(javaData.toMap)

  def idField = ClientFields.USER

  def pwdHashField = ClientFields.PWDHASH

  // default true if field missing
  def isLegacyTokens: Boolean = ! get(ClientFields.LEGACY_TOKENS).exists(java.lang.Boolean.valueOf(_) == false)
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

  val LEGACY_TOKENS = new ClientEnumVal { def name = "legacy_tokens"; override def required = false }

}
