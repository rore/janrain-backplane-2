package com.janrain.backplane.server;

import com.janrain.backplane.server.config.Backplane1Config;
import com.janrain.backplane.server.migrate.legacy.BackplaneMessage;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.crypto.ChannelUtil;
import org.apache.log4j.Logger;

import java.io.*;
import java.text.ParseException;
import java.util.Date;
import java.util.LinkedHashMap;
import java.util.Map;

/**
 * @author Tom Raney
 */
public class BackplaneMessageNew implements Serializable {

    public BackplaneMessageNew() {};

    public BackplaneMessageNew(BackplaneMessage oldMessage) {
        id = oldMessage.get(BackplaneMessage.Field.ID.getFieldName());
        bus = oldMessage.get(BackplaneMessage.Field.BUS.getFieldName());
        channel = oldMessage.get(BackplaneMessage.Field.CHANNEL_NAME.getFieldName());
        payload = oldMessage.get(BackplaneMessage.Field.PAYLOAD.getFieldName());
        source = oldMessage.get(BackplaneMessage.Field.SOURCE.getFieldName());
        type = oldMessage.get(BackplaneMessage.Field.TYPE.getFieldName());
    }

    public BackplaneMessage convertToOld() throws SimpleDBException, BackplaneServerException {
        Map<String,Object> d = new LinkedHashMap<String, Object>();
        d.put(BackplaneMessage.Field.STICKY.getFieldName(), Boolean.toString(this.sticky));
        d.put(BackplaneMessage.Field.SOURCE.getFieldName(), source);
        d.put(BackplaneMessage.Field.PAYLOAD.getFieldName(), payload);
        d.put(BackplaneMessage.Field.TYPE.getFieldName(), type);

        return new BackplaneMessage(id, bus, channel, d);
    }

    public String getId() {
        return id;
    }

    public void setId(String id) {
        this.id = id;
    }

    public String getBus() {
        return bus;
    }

    public void setBus(String bus) {
        this.bus = bus;
    }

    public String getChannel() {
        return channel;
    }

    public void setChannel(String channel) {
        this.channel = channel;
    }

    public String getPayload() {
        return payload;
    }

    public void setPayload(String payload) {
        this.payload = payload;
    }

    public boolean isSticky() {
        return sticky;
    }

    public void setSticky(boolean sticky) {
        this.sticky = sticky;
    }

    public String getSource() {
        return source;
    }

    public void setSource(String source) {
        this.source = source;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public Date getReceived() {
        return this.received;
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

    public static BackplaneMessageNew fromBytes(byte[] bytes) {

        if (bytes == null) {
            return null;
        }

        ObjectInputStream in = null;
        try {
            in = new ObjectInputStream(new ByteArrayInputStream(bytes));
            return (BackplaneMessageNew) in.readObject();
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

    public static Date getDateFromId(String backplaneMessageId) {
        if (backplaneMessageId == null) {
            return null;
        }

        try {
            return Backplane1Config.ISO8601.parse(backplaneMessageId.substring(0, backplaneMessageId.indexOf("Z")+1));
        } catch (ParseException e) {
            logger.warn(e);
        }
        return null;
    }

    /**
     * @return a time-based, lexicographically comparable message ID.
     */
    public static String generateMessageId() {
        return Backplane1Config.ISO8601.format(new Date()) + "-" + ChannelUtil.randomString(10);
    }

    /**
     * @return a time-based, lexicographically comparable message ID.
     */
    public static String generateMessageId(Date date) {
        return Backplane1Config.ISO8601.format(date) + "-" + ChannelUtil.randomString(10);
    }

    // - PRIVATE

    private static final Logger logger = Logger.getLogger(BackplaneMessageNew.class);

    private Date received = new Date();
    private String id;
    private String bus;
    private String channel;
    private String payload;
    private boolean sticky=false;
    private String source;
    private String type;

}
