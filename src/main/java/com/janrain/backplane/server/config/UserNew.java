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

import com.janrain.backplane.server.migrate.legacy.User;
import com.janrain.commons.supersimpledb.SimpleDBException;
import org.apache.log4j.Logger;

import java.io.*;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * @author Tom Raney
 */
public class UserNew implements Serializable {

    public UserNew() {};

    public UserNew(User old) {
        this.id = old.get(User.Field.USER);
        this.user = old.get(User.Field.USER);
        this.pwdHash = old.get(User.Field.PWDHASH);
    }

    public User convertToOld() throws SimpleDBException {
        Map<String,String> d = new LinkedHashMap<String, String>();
        d.put(User.Field.USER.getFieldName(), this.user);
        d.put(User.Field.PWDHASH.getFieldName(), this.pwdHash);

        User user = new User();
        user.putAll(d);
        user.setName(this.id);

        return user;
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getPwdHash() {
        return pwdHash;
    }

    public void setPwdHash(String pwdHash) {
        this.pwdHash = pwdHash;
    }

    public String getUser() {
        return user;
    }

    public void setUser(String user) {
        this.user = user;
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

    public static UserNew fromBytes(byte[] bytes) {

        if (bytes == null) {
            return null;
        }

        ObjectInputStream in = null;
        try {
            in = new ObjectInputStream(new ByteArrayInputStream(bytes));
            return (UserNew) in.readObject();
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

    // PRIVATE

    private static final Logger logger = Logger.getLogger(UserNew.class);

    private String id;
    private String pwdHash;
    private String user;
}
