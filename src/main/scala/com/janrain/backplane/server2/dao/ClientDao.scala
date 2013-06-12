package com.janrain.backplane.server2.dao

import com.janrain.backplane.dao.Dao
import com.janrain.backplane.server2.model.{ClientFields, Client}
import com.janrain.backplane.common.UserPassAuth

/**
 * @author Johnny Bufu
 */
trait ClientDao extends Dao[Client] with UserPassAuth[ClientFields.EnumVal, Client] {
  final override val pwdHashField = ClientFields.PWDHASH
}
