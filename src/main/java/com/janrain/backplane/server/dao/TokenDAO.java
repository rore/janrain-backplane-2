package com.janrain.backplane.server.dao;

import com.janrain.backplane.server.Token;
import com.janrain.backplane.server.config.BackplaneConfig;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import org.apache.log4j.Logger;
import org.springframework.stereotype.Service;

import javax.inject.Inject;


/**
 * @author Tom Raney
 */

public class TokenDAO extends DAO {

    TokenDAO(SuperSimpleDB superSimpleDB, BackplaneConfig bpConfig) {
        super(superSimpleDB, bpConfig);
    };

    public void persistToken(Token token) throws SimpleDBException {
        superSimpleDB.store(bpConfig.getAccessTokenTableName(), Token.class, token);
    }

    public Token retrieveToken(String token) throws SimpleDBException {
        return superSimpleDB.retrieve(bpConfig.getAccessTokenTableName(), Token.class, token);
    }


}
