package com.janrain.backplane.server.config;

import com.janrain.backplane.server.ExternalizableCore;
import com.janrain.commons.supersimpledb.SimpleDBException;
import com.janrain.commons.supersimpledb.message.MessageField;
import org.apache.log4j.Logger;

import java.util.EnumSet;
import java.util.LinkedHashMap;
import java.util.Map;
import java.util.Set;

/**
 * @author Tom Raney
 */
public class BpServerConfig extends ExternalizableCore {

    public final static String BPSERVER_CONFIG_KEY = "bpserverconfig";

    public BpServerConfig() {
        Map<String,String> d = new LinkedHashMap<String, String>();

        d.put(Field.DEBUG_MODE.getFieldName(), Field.DEBUG_MODE_DEFAULT.toString() );
        d.put(Field.CLEANUP_INTERVAL_MINUTES.getFieldName(), Long.toString(Field.CLEANUP_INTERVAL_MINUTES_DEFAULT));
        d.put(Field.DEFAULT_MESSAGES_MAX.getFieldName(), Long.toString(Field.MESSAGES_MAX_DEFAULT));
        d.put(Field.CONFIG_CACHE_AGE_SECONDS.getFieldName(), Long.toString(Field.CONFIG_CACHE_AGE_SECONDS_DEFAULT));

        try {
            super.init("foo", d);
        } catch (SimpleDBException e) {
            logger.error(e);
        }

    }

    public long getMaxCacheAge() {
        return Long.parseLong(get(Field.CONFIG_CACHE_AGE_SECONDS));
    }

    @Override
    public String getIdValue() {
        return "foo";
    }

    @Override
    public Set<? extends MessageField> getFields() {
        return EnumSet.allOf(Field.class);
    }

    public static enum Field implements MessageField {

        ID,
        DEBUG_MODE,
        CONFIG_CACHE_AGE_SECONDS,
        CLEANUP_INTERVAL_MINUTES,
        DEFAULT_MESSAGES_MAX;

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

        // PRIVATE

        private static final Boolean DEBUG_MODE_DEFAULT = true;
        private static final long CONFIG_CACHE_AGE_SECONDS_DEFAULT = 10;
        private static final long CLEANUP_INTERVAL_MINUTES_DEFAULT = 2;
        private static final long MESSAGES_MAX_DEFAULT = 50;
        private static final long MESSAGE_CACHE_MAX_MB_DEFAULT = 0;
        private static final long TOKEN_CACHE_MAX_MB_DEFAULT = 100;


    }

    // PRIVATE

    private static final long serialVersionUID = -1845727748278295636L;

    private static final Logger logger = Logger.getLogger(BpServerConfig.class);
}
