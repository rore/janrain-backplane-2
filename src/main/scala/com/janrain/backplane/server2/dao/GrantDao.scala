package com.janrain.backplane.server2.dao

import com.janrain.backplane.dao.DaoAll
import com.janrain.backplane2.server.{GrantBuilder, Scope}
import scala.collection.JavaConversions
import com.janrain.util.Loggable
import com.janrain.backplane.server2.oauth2.model.{GrantFields, Grant}
import com.janrain.backplane.server2.model.BackplaneMessageFields

/**
 * @author Johnny Bufu
 */
trait GrantDao extends DaoAll[Grant] with Loggable {

  def getByClientId(clientId: String) = getAll.filter(_.get(GrantFields.ISSUED_TO_CLIENT_ID).exists(_ == clientId))

  /**
   * Revokes buses across the provided grants.
   * Not atomic, best effort.
   * Stops on first error and reports error, even though some grants may have been updated.
   *
   * @return true if any of the existing grants were updated, false if nothing was updated
   */
  def revokeBuses(grants: List[Grant], buses: List[String]): Boolean = {
    val busesToRevoke = new Scope(Scope.getEncodedScopesAsString(BackplaneMessageFields.BUS, JavaConversions.seqAsJavaList(buses)))
    val updatedGrantIds = grants.withFilter(revokeBusesFromGrant(_, busesToRevoke)).map(_.id).toSet
    if ( ! updatedGrantIds.isEmpty ) {
      // revoke (delete) affected tokens
      BP2DAOs.tokenDao.getAll
        .withFilter( t => ! (t.backingGrants.toSet & updatedGrantIds).isEmpty)
        .foreach(t => BP2DAOs.tokenDao.delete(t.id))
    }
    ! updatedGrantIds.isEmpty
  }

  def deleteByBus(busesToDelete: List[String]) {
    val deleteBusesScope: Scope = new Scope(Scope.getEncodedScopesAsString(BackplaneMessageFields.BUS, JavaConversions.seqAsJavaList(busesToDelete)))
    getAll.foreach(grant => {
      Option(JavaConversions.asScalaSet(grant.getAuthorizedScope.getScopeFieldValues(BackplaneMessageFields.BUS)))
        .flatten.foreach(bus =>
          if (busesToDelete.contains(bus)) revokeBusesFromGrant(grant, deleteBusesScope)
        )
    })
  }

  private def revokeBusesFromGrant(grant: Grant, busesToRevoke: Scope): Boolean = {
    val grantScope = grant.getAuthorizedScope
    val updatedScope = Scope.revoke(grantScope, busesToRevoke)
    if (updatedScope == grantScope) return false

    if (!updatedScope.isAuthorizationRequired) {
      logger.info("Revoked all buses from grant: " + grant.id)
      delete(grant.id)
    } else {
      val updated: Grant = new GrantBuilder(grant, grant.getState).scope(updatedScope).buildGrant
      update(grant, updated)
      logger.info("Buses updated updated for grant: " + updated.id + " remaining scope: '" + updated.getAuthorizedScope + "'")
    }
    true
  }
}
