package com.janrain.backplane.server.dao;

import com.janrain.backplane.server.config.BackplaneConfig;
import com.janrain.commons.supersimpledb.SuperSimpleDB;

/**
 * @author Tom Raney
 */

public class DAO {

    protected SuperSimpleDB superSimpleDB;
    protected BackplaneConfig bpConfig;

    DAO(SuperSimpleDB superSimpleDB, BackplaneConfig bpConfig) {
        this.superSimpleDB = superSimpleDB;
        this.bpConfig = bpConfig;
    };
}
