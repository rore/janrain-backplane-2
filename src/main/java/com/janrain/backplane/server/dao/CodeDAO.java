package com.janrain.backplane.server.dao;

import com.janrain.backplane.server.Code;
import com.janrain.backplane.server.config.BackplaneConfig;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;

/**
 * @author Tom Raney
 */

public class CodeDAO extends DAO {

    CodeDAO(SuperSimpleDB superSimpleDB, BackplaneConfig bpConfig) {
        super(superSimpleDB, bpConfig);
    };

    public void persistCode(Code code) throws SimpleDBException {
        superSimpleDB.store(bpConfig.getCodeTableName(), Code.class, code);
    }

    public Code retrieveCode(String code) throws SimpleDBException {
        return superSimpleDB.retrieve(bpConfig.getCodeTableName(), Code.class, code);
    }
}
