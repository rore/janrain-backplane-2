package com.janrain.backplane.server2.dao

import com.janrain.backplane.dao.DaoAll
import com.janrain.backplane.server2.model.{BusOwnerFields, BusOwner}
import com.janrain.backplane.common.UserPassAuth

/**
 * @author Johnny Bufu
 */
trait BusOwnerDao extends DaoAll[BusOwner] with UserPassAuth[BusOwnerFields.EnumVal, BusOwner]
