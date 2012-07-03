package com.janrain.backplane2.server.provision;

import com.janrain.backplane2.server.dao.DAOFactory;
import com.janrain.commons.supersimpledb.message.AbstractMessage;

/**
 * @author Johnny Bufu
 */
public interface ProvisioningConfig {

    /** enforce DB-related constraints on fields */
    void validate(DAOFactory daoFactory) throws Exception;

}
