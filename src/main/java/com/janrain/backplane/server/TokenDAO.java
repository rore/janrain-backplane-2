package com.janrain.backplane.server;

import com.janrain.backplane.server.config.BackplaneConfig;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import org.apache.log4j.Logger;
import org.springframework.stereotype.Service;

import javax.inject.Inject;


/**
 * @author Tom Raney
 */

@Service(value="tokenDao")
public class TokenDAO {

    TokenDAO() {};

    public void persistToken(Token token) throws SimpleDBException {
        superSimpleDB.store(bpConfig.getAccessTokenTableName(), Token.class, token);
    }

    public Token retrieveToken(String token) throws SimpleDBException {
        return superSimpleDB.retrieveAndDelete(bpConfig.getAccessTokenTableName(), Token.class, token);
    }

    private static final Logger logger = Logger.getLogger(TokenDAO.class);

    @Inject
    private SuperSimpleDB superSimpleDB;

    @Inject
    private BackplaneConfig bpConfig;
}
