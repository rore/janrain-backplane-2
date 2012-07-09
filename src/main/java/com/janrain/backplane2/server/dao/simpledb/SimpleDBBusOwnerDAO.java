package com.janrain.backplane2.server.dao.simpledb;

import com.janrain.backplane2.server.BackplaneServerException;
import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.backplane2.server.config.User;
import com.janrain.backplane2.server.dao.BusOwnerDAO;
import com.janrain.backplane2.server.dao.DAOFactory;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import com.janrain.oauth2.TokenException;
import org.apache.log4j.Logger;

import java.util.List;

import static com.janrain.backplane2.server.config.Backplane2Config.SimpleDBTables.BP_BUS_OWNERS;

/**
 * @author Johnny Bufu
 */
public class SimpleDBBusOwnerDAO implements BusOwnerDAO {

    SimpleDBBusOwnerDAO(SuperSimpleDB superSimpleDB, Backplane2Config bpConfig, DAOFactory daoFactory) {
        this.daoFactory = daoFactory;
        this.bpConfig = bpConfig;
        this.superSimpleDB = superSimpleDB;
    }

    @Override
    public User get(String id) throws BackplaneServerException {
        try {
            return superSimpleDB.retrieve(bpConfig.getTableName(BP_BUS_OWNERS), User.class, id);
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }

    @Override
    public List<User> getAll() throws BackplaneServerException {
        try {
            return superSimpleDB.retrieveAll(bpConfig.getTableName(BP_BUS_OWNERS), User.class);
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }

    @Override
    public void persist(User user) throws BackplaneServerException {
        try {
            superSimpleDB.store(bpConfig.getTableName(BP_BUS_OWNERS), User.class, user);
        } catch (SimpleDBException e) {
            throw new BackplaneServerException(e.getMessage());
        }
    }

    /** Deletes bus owner; associated buses, grants and tokens issued against the grants are also deleted. */
    @Override
    public void delete(String id) throws BackplaneServerException, TokenException {
        try {
            logger.info("=== BEGIN BUS OWNER DELETE ===");
            superSimpleDB.delete(bpConfig.getTableName(BP_BUS_OWNERS), id);
            // delete all associated buses (and their dependencies)
            daoFactory.getBusDao().deleteByOwner(id);
            logger.info("Bus owner " + id + " deleted successfully");
            logger.info("=== END BUS OWNER DELETE ===");
        } catch (SimpleDBException e) {
            logger.error("An exception occurred during an atomic operation.  Corruption may have occurred while removing bus owner: " + id);
            throw new BackplaneServerException(e.getMessage());
        }
    }

    // - PRIVATE

    private static final Logger logger = Logger.getLogger(SimpleDBBusOwnerDAO.class);

    private final DAOFactory daoFactory;
    private final SuperSimpleDB superSimpleDB;
    private final Backplane2Config bpConfig;

}
