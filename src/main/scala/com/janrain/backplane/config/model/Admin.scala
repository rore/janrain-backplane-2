package com.janrain.backplane.config.model

import org.springframework.aop.interceptor.AbstractMonitoringInterceptor
import com.janrain.backplane.common.model.{UserFieldEnum, User}

/**
 * @author Johnny Bufu
 */
class Admin(data: Map[String,String]) extends User("admin", data, AdminFields.values) {

  def this(username: String, pwdhash: String) = this(Map(
    AdminFields.USER.name -> username,
    AdminFields.PWDHASH.name -> pwdhash
  ))

  def idField = AdminFields.USER
}

object AdminFields extends UserFieldEnum {
  trait AdminEnumVal extends EnumVal
  // no new, admin-specific fields
}
