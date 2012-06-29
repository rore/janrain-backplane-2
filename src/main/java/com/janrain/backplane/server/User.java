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

package com.janrain.backplane.server;

import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.message.AbstractMessage;
import com.janrain.commons.supersimpledb.message.MessageField;
import org.apache.log4j.Logger;

import java.io.*;
import java.util.*;

/**
 * @author Johnny Bufu
 */
public class User extends AbstractMessage implements Externalizable {

    // - PUBLIC

    @Override
    public String getIdValue() {
        return get(Field.USER);
    }

    @Override
    public Set<? extends MessageField> getFields() {
        return EnumSet.allOf(Field.class);
    }

    @Override
    public void writeExternal(ObjectOutput objectOutput) throws IOException {
        HashMap<String, String> map = new HashMap<String, String>();
        Set<String> keys = this.keySet();
        Iterator it = keys.iterator();
        while (it.hasNext()) {
            String key = (String) it.next();
            map.put(key, this.get(key));
        }

        objectOutput.writeObject(map);
    }

    @Override
    public void readExternal(ObjectInput objectInput) throws IOException, ClassNotFoundException {
        this.putAll((Map<? extends String, ? extends String>) objectInput.readObject());
    }

    public static enum Field implements MessageField {

        // - PUBLIC

        USER,
        PWDHASH;

        @Override
        public String getFieldName() {
            return name();
        }

        @Override
        public boolean isRequired() {
            return true;
        }

        @Override
        public void validate(String value) throws SimpleDBException {
            if (isRequired()) validateNotBlank(name(), value);
        }
    }


    public byte[] toBytes() {
        ByteArrayOutputStream bos = new ByteArrayOutputStream();
        ObjectOutputStream oos = null;
        byte[] bytes = null;
        try {
            oos = new ObjectOutputStream(bos);
            oos.writeObject(this);
            oos.flush();
            bytes = bos.toByteArray();
        } catch (IOException e) {
            logger.error(e);
        } finally {
            try {
                if (oos != null) {
                    oos.close();
                }
                bos.close();
            } catch (IOException e) {
                logger.error(e);
            }
        }
        return bytes;
    }

    public static User fromBytes(byte[] bytes) {

        if (bytes == null) {
            return null;
        }

        ObjectInputStream in = null;
        try {
            in = new ObjectInputStream(new ByteArrayInputStream(bytes));
            return (User) in.readObject();
        } catch (Exception e1) {
            logger.error(e1);
        } finally {
            try {
                if (in != null) {
                    in.close();
                }
            } catch (IOException e) {
                logger.error(e);
            }
        }
        return null;
    }


    // PRIVATE

    private static final long serialVersionUID = -2193539128976254287L;

    private static final Logger logger = Logger.getLogger(User.class);
}
