package com.janrain.backplane2.server;

import com.janrain.backplane.server.ExternalizableCore;
import com.janrain.backplane2.server.config.BusConfig2;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.message.MessageField;
import com.janrain.crypto.ChannelUtil;

import java.util.EnumSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

/**
 * @author Johnny Bufu
 */
public class Channel extends ExternalizableCore {

    @SuppressWarnings("UnusedDeclaration")
    public Channel() { }

    public Channel(String channelId, BusConfig2 busConfig, int channelExpireSeconds) throws SimpleDBException {
        String id = channelId == null ? ChannelUtil.randomString(CHANNEL_NAME_LENGTH) : channelId;
        Map<String,String> data = new LinkedHashMap<String, String>();
        data.put(ChannelField.ID.getFieldName(), id);
        data.put(ChannelField.BUS.getFieldName(), busConfig.getIdValue());
        data.put(ChannelField.EXPIRE_SECONDS.getFieldName(), Integer.toString(channelExpireSeconds));
        data.put(ChannelField.MESSAGE_EXPIRE_DEFAULT_SECONDS.getFieldName(), Integer.toString(busConfig.getRetentionTimeSeconds()));
        data.put(ChannelField.MESSAGE_EXPIRE_MAX_SECONDS.getFieldName(), Integer.toString(busConfig.getRetentionTimeStickySeconds()));
        super.init(id, data);
    }

    @Override
    public String getIdValue() {
        return get(ChannelField.ID);
    }

    @Override
    public Set<? extends MessageField> getFields() {
        return EnumSet.allOf(ChannelField.class);
    }

    public static enum ChannelField implements MessageField {

        // - PUBLIC

        ID("id"),

        BUS("bus"),

        EXPIRE_SECONDS("expire_seconds") {
            @Override
            public void validate(String value) throws SimpleDBException {
                super.validate(value);
                validateInt(getFieldName(), value);
        }},

        MESSAGE_EXPIRE_DEFAULT_SECONDS("message_expire_default_seconds") {
            @Override
            public void validate(String value) throws SimpleDBException {
                super.validate(value);
                validateInt(getFieldName(), value);
        }},

        MESSAGE_EXPIRE_MAX_SECONDS("message_expire_max_seconds") {
            @Override
            public void validate(String value) throws SimpleDBException {
                super.validate(value);
                validateInt(getFieldName(), value);
        }};

        @Override
        public String getFieldName() {
            return fieldName;
        }

        @Override
        public boolean isRequired() {
            return true;
        }

        @Override
        public void validate(String value) throws SimpleDBException {
            if (isRequired()) validateNotBlank(getFieldName(), value);
        }

        // - PRIVATE

        private String fieldName;

        private ChannelField(String fieldName) {
            this.fieldName = fieldName;
        }
    }

    private static final int CHANNEL_NAME_LENGTH = 32;

}
