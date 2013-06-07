package com.janrain.backplane.server2.dao

import com.janrain.backplane.dao.redis.RedisMessageDao
import com.janrain.backplane.server2.oauth2.model.{AuthorizationDecisionKey, AuthorizationRequest}
import com.janrain.backplane.dao.ExpiringDao
import com.janrain.backplane.server2.model.AuthSession


/**
 * @author Johnny Bufu
 */
object BP2DAOs {

  // todo: review key prefixes

  val authSessionDao: AuthSessionDao = new RedisMessageDao[AuthSession]("authSession:")
    with AuthSessionDao
    with ExpiringDao[AuthSession] {
    protected def instantiate(data: Map[_, _]) = new AuthSession(data.map(kv => (kv._1.toString -> kv._2.toString) ))
    val expireSeconds = 3600 // 1h
  }

  val authorizationRequestDao: AuthorizationRequestDao = new RedisMessageDao[AuthorizationRequest]("authorizationRequest:")
    with AuthorizationRequestDao
    with ExpiringDao[AuthorizationRequest] {
    protected def instantiate(data: Map[_, _]) = new AuthorizationRequest(data.map(kv => (kv._1.toString -> kv._2.toString) ))

    val expireSeconds = 300 // 5min
  }

  val authorizationDecisionKeyDao: AuthorizationDecisionKeyDao = new RedisMessageDao[AuthorizationDecisionKey]("authorizationDecisionKey:")
    with AuthorizationDecisionKeyDao
    with ExpiringDao[AuthorizationDecisionKey] {
    protected def instantiate(data: Map[_, _]) = new AuthorizationDecisionKey(data.map(kv => (kv._1.toString -> kv._2.toString) ))

    val expireSeconds = 300 // 5min
  }

}
