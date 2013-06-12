package com.janrain.backplane.server1.model

import com.janrain.backplane.common.model.{UserFieldEnum, User}

/**
 * @author Johnny Bufu
 */
class BusUser(data: Map[String,String]) extends User("bp1User", data, BusUserFields.values) {

  def this(username: String, pwdhash: String) = this(Map(
    BusUserFields.USER.name -> username,
    BusUserFields.PWDHASH.name -> pwdhash
  ))

  def idField = BusUserFields.USER
}

object BusUserFields extends UserFieldEnum {
  trait BusUserEnumVal extends EnumVal
  // no new, BusUser-specific fields
}
