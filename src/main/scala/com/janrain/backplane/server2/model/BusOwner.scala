package com.janrain.backplane.server2.model

import com.janrain.backplane.common.model.{UserFieldEnum, User}

/**
 * @author Johnny Bufu
 */
class BusOwner(data: Map[String,String]) extends User("bp2BusOwner", data, BusOwnerFields.values) {

  def this(username: String, pwdhash: String) = this(Map(
    BusOwnerFields.USER.name -> username,
    BusOwnerFields.PWDHASH.name -> pwdhash
  ))

  def idField = BusOwnerFields.USER
}

object BusOwnerFields extends UserFieldEnum {
  trait BusOwnerEnumVal extends EnumVal
  // no new, BusOwner.-specific fields
}
