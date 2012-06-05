/*
 * Copyright 2012 Janrain, Inc.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *    http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

package com.janrain.backplane.server.dao;

import com.janrain.backplane.server.BackplaneMessage;
import com.janrain.backplane.server.config.Backplane1Config;
import com.janrain.cache.CachedMemcached;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.SuperSimpleDB;
import org.apache.commons.lang.StringUtils;

import java.util.*;

/**
 * @author Tom Raney
 */
public class BackplaneMessageDAO extends DAO<BackplaneMessage> {

    @Override
    public void persist(BackplaneMessage message) throws SimpleDBException {
        superSimpleDB.store(bpConfig.getMessagesTableName(), BackplaneMessage.class, message, true);

        // update the cache
        String channel = message.get(BackplaneMessage.Field.CHANNEL_NAME.getFieldName());
        List<BackplaneMessage> messages = getMessagesByChannel(channel, null, null);
        messages.add(message);
        CachedMemcached.getInstance().setObject(genChannelKey(channel), 3600, messages);

        //update the bus list too
        String bus = message.get(BackplaneMessage.Field.BUS.getFieldName());
        messages = getMessagesByBus(bus, null, null);
        messages.add(message);
        CachedMemcached.getInstance().setObject(genBusKey(bus), 3600, messages);

    }

    @Override
    public void delete(String id) throws SimpleDBException {

    }

    /**
     * Fetch a list (possibly empty) of backplane messages that exist on the channel
     * and return them in order by message id
     * @param channel
     * @return
     */

    public List<BackplaneMessage> getMessagesByChannel(String channel, String since, String sticky) throws SimpleDBException {

        //check the cache first
        List<BackplaneMessage> messages = (List<BackplaneMessage>) CachedMemcached.getInstance().getObject(genChannelKey(channel));

        if (messages == null) {
            //check the DB
            StringBuilder whereClause = new StringBuilder()
                    .append(BackplaneMessage.Field.CHANNEL_NAME.getFieldName()).append("='").append(channel).append("'");

            messages = superSimpleDB.retrieveWhere(bpConfig.getMessagesTableName(), BackplaneMessage.class, whereClause.toString(), true);

            CachedMemcached.getInstance().setObject(genChannelKey(channel), 3600, messages);
        }

        return filterAndSort(messages, since, sticky);

    }

    public List<BackplaneMessage> getMessagesByBus(String bus, String since, String sticky) throws SimpleDBException {

        //check the cache first
        List<BackplaneMessage> messages = (List<BackplaneMessage>) CachedMemcached.getInstance().getObject(genBusKey(bus));

        if (messages == null) {
            //check the DB
            StringBuilder whereClause = new StringBuilder()
                    .append(BackplaneMessage.Field.BUS.getFieldName()).append("='").append(bus).append("'");

            messages = superSimpleDB.retrieveWhere(bpConfig.getMessagesTableName(), BackplaneMessage.class, whereClause.toString(), true);

            CachedMemcached.getInstance().setObject(genBusKey(bus), 3600, messages);
        }

        return filterAndSort(messages, since, sticky);

    }

    // - PACKAGE

    BackplaneMessageDAO(SuperSimpleDB superSimpleDB, Backplane1Config bpConfig, com.janrain.backplane.server.dao.DaoFactory daoFactory) {
        super(superSimpleDB, bpConfig);
        this.daoFactory = daoFactory;
    }

    // - PRIVATE

    private final DaoFactory daoFactory;

    private List<BackplaneMessage> filterAndSort(List<BackplaneMessage> messages, String since, String sticky) {

        // filter per sticky flag
        if (StringUtils.isNotBlank(sticky)) {
            for (BackplaneMessage message: messages) {
                if (!message.get(BackplaneMessage.Field.STICKY.getFieldName()).equals(sticky)) {
                    messages.remove(message);
                }
            }
        }

        // filter per since flag
        if (StringUtils.isNotBlank(since)) {
            List<BackplaneMessage> newList = new ArrayList<BackplaneMessage>();
            for (BackplaneMessage msg: messages) {
                if (msg.getIdValue().compareTo(since) > 0) {
                    newList.add(msg);
                }
            }
            messages = newList;
        }

        Collections.sort(messages, new Comparator<BackplaneMessage>() {
            @Override
            public int compare(BackplaneMessage backplaneMessage, BackplaneMessage backplaneMessage1) {
                if (backplaneMessage == backplaneMessage1) {
                    return 0;
                }
                return backplaneMessage.getIdValue().compareTo(backplaneMessage1.getIdValue());
            }
        });

        return messages;

    }

    private String genBusKey(String key) {
        return "v1bus" + key;
    }

    private String genChannelKey(String key) {
        return "v1channel" + key;
    }
}
