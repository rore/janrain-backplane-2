package com.janrain.utils;

import com.janrain.commons.util.InitSystemProps;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;

import javax.naming.InitialContext;
import java.net.InetAddress;
import java.net.UnknownHostException;

/**
 * @author Tom Raney
 */
public class BackplaneSystemProps extends InitSystemProps {

    public static final String GRAPHITE_SERVER    = "GRAPHITE_SERVER";
    public static final String ZOOKEEPER_SERVERS  = "ZOOKEEPER_SERVERS";
    public static final String REDIS_SERVER_PRIMARY = "REDIS_SERVER_PRIMARY";
    public static final String REDIS_SERVER_SECONDARY = "REDIS_SERVER_SECONDARY";
    public static final String IP_WHITE_LIST = "IP_WHITE_LIST";

    public static String getMachineName() {
        try {
            return "backplane/" + InetAddress.getLocalHost().getHostName();
        }  catch (UnknownHostException e) {
            logger.warn("get localhost call failed");
        }
        return "n/a";
    }

    public BackplaneSystemProps(String log4jFile) {
        super(log4jFile);

        load(GRAPHITE_SERVER, false);
        load(ZOOKEEPER_SERVERS, true);
        load(REDIS_SERVER_PRIMARY, true);
        load(REDIS_SERVER_SECONDARY, true);
        load(IP_WHITE_LIST, false);

    }

    // - PRIVATE

    private static final Logger logger = Logger.getLogger(BackplaneSystemProps.class);

    private void load(String paramName, boolean required) {
        String result = System.getProperty(paramName);
        if (StringUtils.isBlank(result)) {
            try {
                javax.naming.Context initCtx = new InitialContext();
                result = (String) initCtx.lookup("java:comp/env/" + paramName);
                System.setProperty(paramName, result);
                System.out.println("Parameter " + paramName + " fetched from context and inserted as system property");
            } catch (Exception e) {
                //continue
                if (required) {
                    System.out.println("An error occurred trying to locate required parameter " + paramName + " => " + e.getMessage());
                }
            }
        } else {
            System.out.println("Parameter " + paramName + " exists as a system property");
        }
    }


}
