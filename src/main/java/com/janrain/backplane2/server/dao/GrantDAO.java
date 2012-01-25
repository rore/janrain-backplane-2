package com.janrain.backplane2.server.dao;

import com.janrain.backplane2.server.AuthCode;
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
        // delete associated Code, if one exists
        superSimpleDB.delete(bpConfig.getCodeTableName(), grantId);
    }

    public AuthCode issueCode(Grant grant) throws SimpleDBException {
        AuthCode code = new AuthCode(grant.getIdValue(),
                (grant.getExpiresDate()==null?new Date(new Date().getTime() + AuthCode.EXPIRES_SECONDS * 1000L): grant.getExpiresDate()));
        superSimpleDB.store(bpConfig.getCodeTableName(), AuthCode.class, code);
        return code;
    }

    public AuthCode retrieveCode(String codeId) throws SimpleDBException {
        AuthCode code = superSimpleDB.retrieve(bpConfig.getCodeTableName(), AuthCode.class, codeId);
        code.setGrant(retrieveGrant(code.getGrantId()));
        return code;
    }

    public void persistCode(AuthCode code) throws SimpleDBException {
        superSimpleDB.store(bpConfig.getCodeTableName(), AuthCode.class, code);
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
