package com.janrain.backplane.server2.dao

import com.janrain.commons.supersimpledb.message.NamedMap

/**
 * @author Johnny Bufu
 */
trait LegacySupport[LT <: NamedMap] {

  def asLegacy: LT

}
