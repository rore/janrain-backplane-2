package com.janrain.backplane.server1.dao

import com.janrain.backplane.dao.redis.RedisMessageDao
import com.janrain.backplane.server1.model.{BusUserFields, BusConfig1, BusUser}
import com.janrain.backplane.dao.PasswordHasherDao

/**
 * @author Johnny Bufu
 */
object BP1DAOs {

  val userDao: BusUserDao = new RedisMessageDao[BusUser]("bp1BusUser:") with BusUserDao with PasswordHasherDao[BusUserFields.EnumVal,BusUser] {
    protected def instantiate(data: Map[_, _]) = new BusUser( data.map( kv => kv._1.toString -> kv._2.toString ))
  }

  val busDao: BusDao = new RedisMessageDao[BusConfig1]("bp1Bus") with BusDao {
    protected def instantiate(data: Map[_, _]) = new BusConfig1( data.map( kv => kv._1.toString -> kv._2.toString ))
  }

}
