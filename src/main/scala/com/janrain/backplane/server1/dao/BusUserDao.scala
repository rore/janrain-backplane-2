package com.janrain.backplane.server1.dao

import com.janrain.backplane.server1.model.{BusUserFields, BusUser}
import com.janrain.backplane.dao.Dao
import com.janrain.backplane.common.UserPassAuth

/**
 * @author Johnny Bufu
 */
trait BusUserDao extends Dao[BusUser] with UserPassAuth[BusUserFields.EnumVal,BusUser] {
  final override val pwdHashField = BusUserFields.PWDHASH
}