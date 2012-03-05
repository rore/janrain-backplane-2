package com.janrain.backplane2.server.provision;

import com.janrain.backplane2.server.dao.DaoFactory;
import com.janrain.commons.supersimpledb.message.AbstractMessage;

/**
 * @author Johnny Bufu
 */
public abstract class ProvisioningConfig extends AbstractMessage {

    /** enforce DB-related constraints on fields */
    public abstract void validate(DaoFactory daoFactory) throws Exception;

}
