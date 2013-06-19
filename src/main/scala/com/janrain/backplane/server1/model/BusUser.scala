package com.janrain.backplane.server1.model

import com.janrain.backplane.common.model.{UserFieldEnum, User}
import com.janrain.backplane.config.model.Password

/**
 * @author Johnny Bufu
 */
class BusUser(data: Map[String,String]) extends User("bp1User", data, BusUserFields.values)
  with Password[BusUserFields.EnumVal, BusUser] {

  def this(username: String, pwdhash: String) = this(Map(
    BusUserFields.USER.name -> username,
    BusUserFields.PWDHASH.name -> pwdhash
  ))

  def idField = BusUserFields.USER

  def pwdHashField = BusUserFields.PWDHASH
}

object BusUserFields extends UserFieldEnum {
  trait BusUserEnumVal extends EnumVal
  // no new, BusUser-specific fields
}
