package com.janrain.backplane.server1.redisdao;

import com.janrain.backplane.common.BackplaneServerException;
import com.janrain.backplane.dao.DAO;
import com.janrain.backplane.server1.BackplaneMessage;
import com.janrain.commons.supersimpledb.SimpleDBException;

import java.util.List;

/**
 * @author Johnny Bufu
 */
public interface BP1MessageDao extends DAO<BackplaneMessage> {

    public List<BackplaneMessage> getMessagesByBus(String bus, String since, String sticky)
            throws SimpleDBException, BackplaneServerException;

    public List<BackplaneMessage> getMessagesByChannel(String bus, String channel, String since, String sticky)
            throws SimpleDBException, BackplaneServerException;

    public int getMessageCount(String bus, String channel);

    public void deleteExpiredMessages();
}
