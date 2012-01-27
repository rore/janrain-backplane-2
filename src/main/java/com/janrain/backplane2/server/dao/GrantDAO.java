package com.janrain.backplane2.server.dao;

import com.janrain.backplane2.server.Grant;
import com.janrain.backplane2.server.Scope;
import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * @author Tom Raney
 */

public class GrantDAO extends DAO {

    GrantDAO(SuperSimpleDB superSimpleDB, Backplane2Config bpConfig) {
        super(superSimpleDB, bpConfig);
    };

    public void persistGrant(Grant grant) throws SimpleDBException {
        superSimpleDB.store(bpConfig.getGrantTableName(), Grant.class, grant);
    }

    public Grant retrieveGrant(String grantId) throws SimpleDBException {
        return superSimpleDB.retrieve(bpConfig.getGrantTableName(), Grant.class, grantId);
    }

    public void deleteGrant(String grantId) throws SimpleDBException {
        superSimpleDB.delete(bpConfig.getGrantTableName(), grantId);
    }

    /**
     * Retrieve a list of grants that encompass the buses requested by a client
     * @param clientId
     * @param scope
     * @return
     * @throws SimpleDBException
     */

    public List<Grant> retrieveGrants(String clientId, Scope scope) throws SimpleDBException {
        List<Grant> allGrants = superSimpleDB.retrieveWhere(bpConfig.getGrantTableName(), Grant.class, "issued_to_client='"+ clientId + "'", true);
        ArrayList<Grant> selectedGrants = new ArrayList<Grant>();
        // if no buses exist in requested scope, return entire list
        if (scope.getBusesInScope().isEmpty()) {
            return allGrants;
        }

        for ( String bus: scope.getBusesInScope()) {
            for ( Grant grant : allGrants) {
                if ( grant.getBusesAsList().contains(bus)) {
                    selectedGrants.add(grant);
                }
            }
        }

        return selectedGrants;
    }
}
