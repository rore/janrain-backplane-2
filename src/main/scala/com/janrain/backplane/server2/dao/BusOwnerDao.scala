package com.janrain.backplane.server2.dao

import com.janrain.backplane.dao.Dao
import com.janrain.backplane.server2.model.{BusOwnerFields, BusOwner}
import com.janrain.backplane.common.UserPassAuth

/**
 * @author Johnny Bufu
 */
trait BusOwnerDao extends Dao[BusOwner] with UserPassAuth[BusOwnerFields.EnumVal, BusOwner] {
  final override val pwdHashField = BusOwnerFields.PWDHASH
}
