package com.janrain.backplane.common

import com.janrain.backplane.common.model.{UserField, User}
import com.janrain.backplane.dao.Dao

/**
 * @author Johnny Bufu
 */
trait UserPassAuth[UF <: UserField, UT <: User[UF]] {

  this: Dao[UT] =>

  val pwdHashField: UF

  def checkAuth(user: String, password: String) {
    if ( ! get(user).exists(f => f.get(pwdHashField).exists(HmacHashUtils.checkHmacHash(password, _))))
      throw new AuthException("access denied")
  }
}
