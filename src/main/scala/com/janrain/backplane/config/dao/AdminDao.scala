package com.janrain.backplane.config.dao

import com.janrain.backplane.dao.Dao
import com.janrain.backplane.config.model.{AdminFields, Admin}
import com.janrain.backplane.common.{HmacHashUtils, AuthException}

/**
 * @author Johnny Bufu
 */
trait AdminDao extends Dao[Admin] {
  def checkAdminAuth(user: String, password: String) {
    if (! get(user).exists(f => f.get(AdminFields.PWDHASH).exists(HmacHashUtils.checkHmacHash(password, _))))
      throw new AuthException("access denied")
  }
}
