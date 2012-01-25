package com.janrain.backplane2.server.dao;

import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.commons.supersimpledb.SuperSimpleDB;

/**
 * @author Tom Raney
 */

public abstract class DAO {

    protected SuperSimpleDB superSimpleDB;
    protected Backplane2Config bpConfig;

    DAO(SuperSimpleDB superSimpleDB, Backplane2Config bpConfig) {
        this.superSimpleDB = superSimpleDB;
        this.bpConfig = bpConfig;
    };
}
