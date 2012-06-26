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

package com.janrain.backplane.server.config;

import com.janrain.backplane.server.migrate.legacy.BusConfig1;
import com.janrain.commons.supersimpledb.SimpleDBException;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;

import java.io.*;
import java.util.*;

/**
 * @author Tom Raney
 */
public class BusConfig1New implements Serializable {

    public BusConfig1New(BusConfig1 old) {
        this.busName = old.get(BusConfig1.Field.BUS_NAME);
        this.retentionTimeSeconds = Integer.parseInt(old.get(BusConfig1.Field.RETENTION_TIME_SECONDS));
        this.retentionStickyTimeSeconds = Integer.parseInt(old.get(BusConfig1.Field.RETENTION_STICKY_TIME_SECONDS));
        this.permissions = new HashMap<String, EnumSet>();
        for (Map.Entry<String,String> entry : old.entrySet()) {
            EnumSet<Backplane1Config.BUS_PERMISSION> result = EnumSet.noneOf(Backplane1Config.BUS_PERMISSION.class);
            String perms = entry.getValue();
            String key = entry.getKey();
            if (StringUtils.isNotBlank(perms) &&
                    !key.equals(BusConfig1.Field.BUS_NAME.getFieldName()) &&
                    !key.equals(BusConfig1.Field.RETENTION_TIME_SECONDS.getFieldName()) &&
                    !key.equals(BusConfig1.Field.RETENTION_STICKY_TIME_SECONDS.getFieldName()) &&
                    !key.equals("ssdb_update_version")) {
                for(String perm : perms.split(",")) {
                    try {
                        result.add(Backplane1Config.BUS_PERMISSION.valueOf(perm));
                    } catch (Exception e) {
                        //ignore
                    }
                }
                permissions.put(entry.getKey(),result);
            }

        }

    }

    public BusConfig1 convertToOld() throws SimpleDBException {
        Map<String,String> d = new LinkedHashMap<String, String>();
        d.put(BusConfig1.Field.BUS_NAME.getFieldName(), this.busName);
        d.put(BusConfig1.Field.RETENTION_TIME_SECONDS.getFieldName(), Integer.toString(this.retentionTimeSeconds));
        d.put(BusConfig1.Field.RETENTION_STICKY_TIME_SECONDS.getFieldName(), Integer.toString(this.retentionStickyTimeSeconds));

        for (Map.Entry<String, EnumSet> entry: this.permissions.entrySet()) {
            if (entry.getKey().equals(BusConfig1.Field.BUS_NAME.getFieldName()) ||
                entry.getKey().equals(BusConfig1.Field.RETENTION_TIME_SECONDS.getFieldName()) ||
                entry.getKey().equals(BusConfig1.Field.RETENTION_STICKY_TIME_SECONDS.getFieldName())) {
                break;
            }
            d.put(entry.getKey(), toCommaDelimitedString(entry.getValue()));
        }

        BusConfig1 busConfig1 = new BusConfig1();
        busConfig1.putAll(d);
        busConfig1.setName(this.busName);

        return busConfig1;
    }

    public EnumSet<Backplane1Config.BUS_PERMISSION> getPermissions(String user) {
        if (!permissions.containsKey(user)) {
            throw new IllegalArgumentException("Invalid user name: " + user);
        }

        return permissions.get(user);

    }

    static <E extends Enum<E>> String toCommaDelimitedString(EnumSet<E> set) {
        if (set == null || set.isEmpty()) {
            return "";
        } else {
            final StringBuilder b = new StringBuilder();
            final Iterator<E> i = set.iterator();
            b.append(i.next());
            for(; i.hasNext(); ) {
                b.append(',').append(i.next());
            }
            return b.toString();
        }
    }

    public byte[] toBytes() {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        byte[] bytes = null;
        try {
            ObjectOutputStream oos = new ObjectOutputStream(bos);
            oos.writeObject(this);
            oos.flush();
            bytes = bos.toByteArray();
            oos.close();
            bos.close();
        } catch (IOException e) {
            logger.error(e);
        }

        return bytes;
    }

    public static BusConfig1New fromBytes(byte[] bytes) {

        if (bytes == null) {
            return null;
        }

        ObjectInputStream in = null;
        try {
            in = new ObjectInputStream(new ByteArrayInputStream(bytes));
            return (BusConfig1New) in.readObject();
        } catch (IOException e1) {
            logger.error(e1);
        } catch (ClassNotFoundException e1) {
            logger.error(e1);
        } finally {
            try {
                in.close();
            } catch (IOException e) {
                logger.error(e);
            }
        }
        return null;
    }

    public String getBusName() {
        return busName;
    }

    public void setBusName(String busName) {
        this.busName = busName;
    }

    public int getRetentionTimeSeconds() {
        return retentionTimeSeconds;
    }

    public void setRetentionTimeSeconds(int retentionTimeSeconds) {
        this.retentionTimeSeconds = retentionTimeSeconds;
    }

    public int getRetentionStickyTimeSeconds() {
        return retentionStickyTimeSeconds;
    }

    public void setRetentionStickyTimeSeconds(int retentionStickyTimeSeconds) {
        this.retentionStickyTimeSeconds = retentionStickyTimeSeconds;
    }

    public Map<String, EnumSet> getPermissions() {
        return permissions;
    }

    public void setPermissions(Map<String, EnumSet> permissions) {
        this.permissions = permissions;
    }

    private static final Logger logger = Logger.getLogger(BusConfig1New.class);

    private String busName;
    private int retentionTimeSeconds;
    private int retentionStickyTimeSeconds;
    private Map<String, EnumSet> permissions;







}
