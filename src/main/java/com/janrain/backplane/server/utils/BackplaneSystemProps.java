package com.janrain.backplane.server.utils;

import com.janrain.commons.util.InitSystemProps;
import org.apache.commons.lang.StringUtils;

import javax.naming.InitialContext;

/**
 * @author Tom Raney
 */
public class BackplaneSystemProps extends InitSystemProps {

    public static final String GRAPHITE_SERVER    = "GRAPHITE_SERVER";
    public static final String ZOOKEEPER_SERVERS  = "ZOOKEEPER_SERVERS";
    public static final String REDIS_SERVER_PRIMARY = "REDIS_SERVER_PRIMARY";

    public BackplaneSystemProps(String log4jFile) {
        super(log4jFile);

        load(GRAPHITE_SERVER);
        load(ZOOKEEPER_SERVERS);
        load(REDIS_SERVER_PRIMARY);

    }

    private void load(String paramName) {
        String result = System.getProperty(paramName);
        if (StringUtils.isBlank(result)) {
            try {
                javax.naming.Context initCtx = new InitialContext();
                result = (String) initCtx.lookup("java:comp/env/" + paramName);
                System.setProperty(paramName, result);
                System.out.println("Parameter " + paramName + " fetched from context and inserted as system property");
            } catch (Exception e) {
                //continue
                System.out.println("An error occurred trying to locate required parameter " + paramName + " => " + e.getMessage());
            }
        } else {
            System.out.println("Parameter " + paramName + " exists as a system property");
        }
    }


}
