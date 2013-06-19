package com.janrain.backplane.config.model

import com.janrain.backplane.common.model.{UserFieldEnum, User}

/**
 * @author Johnny Bufu
 */
class Admin(data: Map[String,String]) extends User("admin", data, AdminFields.values)
  with Password[AdminFields.EnumVal, Admin] {

  def this(username: String, pwdhash: String) = this(Map(
    AdminFields.USER.name -> username,
    AdminFields.PWDHASH.name -> pwdhash
  ))

  def idField = AdminFields.USER

  def pwdHashField = AdminFields.PWDHASH
}

object AdminFields extends UserFieldEnum {
  trait AdminEnumVal extends EnumVal
  // no new, admin-specific fields
}
