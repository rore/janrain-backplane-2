package com.janrain.backplane.server2.dao

import com.janrain.backplane.dao.redis.RedisMessageDao
import com.janrain.backplane.server2.oauth2.model.{AuthorizationDecisionKey, AuthorizationRequest}
import com.janrain.backplane.dao.ExpiringDao
import com.janrain.backplane.server2.model._

/**
 * @author Johnny Bufu
 */
object BP2DAOs {

  val authSessionDao: AuthSessionDao = new RedisMessageDao[AuthSession]("bp2AuthSession:")
    with AuthSessionDao
    with ExpiringDao[AuthSession] {
    protected def instantiate(data: Map[_, _]) = new AuthSession(data.map(kv => kv._1.toString -> kv._2.toString ))
    val expireSeconds = 3600 // 1h
  }

  val authorizationRequestDao: AuthorizationRequestDao = new RedisMessageDao[AuthorizationRequest]("bp2AuthorizationRequest:")
    with AuthorizationRequestDao
    with ExpiringDao[AuthorizationRequest] {
    protected def instantiate(data: Map[_, _]) = new AuthorizationRequest(data.map(kv => kv._1.toString -> kv._2.toString ))

    val expireSeconds = 300 // 5min
  }

  val authorizationDecisionKeyDao: AuthorizationDecisionKeyDao = new RedisMessageDao[AuthorizationDecisionKey]("bp2AuthorizationDecisionKey:")
    with AuthorizationDecisionKeyDao
    with ExpiringDao[AuthorizationDecisionKey] {
    protected def instantiate(data: Map[_, _]) = new AuthorizationDecisionKey(data.map(kv => kv._1.toString -> kv._2.toString ))

    val expireSeconds = 300 // 5min
  }

  val busDao: BusDao = new RedisMessageDao[BusConfig2]("bp2Bus:") with BusDao {
    protected def instantiate(data: Map[_, _]) = new BusConfig2( data.map( kv => kv._1.toString -> kv._2.toString ))
  }

  val busOwnerDao: BusOwnerDao = new RedisMessageDao[BusOwner]("bp2BusOwner:") with BusOwnerDao {
    protected def instantiate(data: Map[_, _]) = new BusOwner( data.map( kv => kv._1.toString -> kv._2.toString ))
  }

  val clientDao: ClientDao = new RedisMessageDao[Client]("bp2Client:") with ClientDao {
    protected def instantiate(data: Map[_, _]) = new Client( data.map( kv => kv._1.toString -> kv._2.toString ))
  }

  val grantDao: GrantDao = new RedisMessageDao[Grant]("bp2Grant:") with GrantDao {
    protected def instantiate(data: Map[_, _]) = new Grant( data.map( kv => kv._1.toString -> kv._2.toString ))
  }

}
