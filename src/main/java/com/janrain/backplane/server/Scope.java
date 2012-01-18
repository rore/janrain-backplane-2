package com.janrain.backplane.server;

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import javax.print.DocFlavor;
import java.lang.reflect.Array;
import java.util.*;

/**
 *
 * @author Tom Raney
 */
public class Scope {

    public static final int MAX_PARAMETERS = 100;
    public static final String SEPARATOR = " ";
    public static final String DELIMITER = ":";

    public static final String[] VALID_KEYS = {"bus","sticky","channel","type","source","payload"};

    public static boolean isValidKey(String key) {
        return Arrays.asList(VALID_KEYS).contains(key);
    }


    public Scope(String scopeString) throws BackplaneServerException {
        this.scopes = parseScopeString(scopeString);
        this.scopeString = scopeString;
        logger.debug("Client requested scopes: " + scopeString);
    }

    /**
     * Return a list of bus names client has requested be in scope
     * @return valid list, possibly empty
     */

    public @NotNull List<String> getBusesInScope() {
        return scopes.get("bus");
    }

    /**
     * Return query string from scope
     * @return
     */
    public String buildQueryFromScope() {
        StringBuilder sb = new StringBuilder();

        boolean firstElement = true;
        for (Map.Entry<String, ArrayList<String>> entry: scopes.entrySet()) {
            if (firstElement != true) {
                sb.append(" AND ");
            }

            if (!entry.getValue().isEmpty()) {
                sb.append("(");
                boolean firstDisjunction = true;
                for (String value : entry.getValue()) {
                    if (firstDisjunction != true) {
                        sb.append(" OR ");
                    }
                    sb.append(entry.getKey() + "='" + value + "'");
                    firstDisjunction = false;
                }
                sb.append(")");
                firstElement = false;
            }
        }

        logger.info("clause = " + sb.toString() + " generated from " + this.scopeString);

        return sb.toString();

    }

    //private

    private String scopeString;
    private Map<String,ArrayList<String>> scopes;
    private static final Logger logger = Logger.getLogger(Scope.class);

    private Map<String,ArrayList<String>> parseScopeString(String scopeString) throws BackplaneServerException {
        HashMap<String,ArrayList<String>> scopes = new HashMap<String, ArrayList<String>>();

        // guarantee bus has an entry
        scopes.put("bus", new ArrayList<String>());

        if (StringUtils.isNotEmpty(scopeString)) {
            // TODO: is there a maximum length for the scope string?
            //
            String[] tokens = scopeString.split(Scope.SEPARATOR,Scope.MAX_PARAMETERS);
            // all scope tokens need to have the ":" key/value delimiter
            for (String token:tokens) {
                if (!token.contains(Scope.DELIMITER)) {
                    logger.debug("Malformed scope: " + scopeString);
                    throw new BackplaneServerException("Malformed scope: " + token);
                }
                String[] keyValue = token.split(Scope.DELIMITER);
                if (!Scope.isValidKey(keyValue[0])) {
                    throw new BackplaneServerException(keyValue[0] + " is not a valid scope field name");
                }

                if (!scopes.containsKey(keyValue[0])) {
                    scopes.put(keyValue[0], new ArrayList<String>());
                }

                List<String> values = scopes.get(keyValue[0]);
                values.add(keyValue[1]);
            }
        }
        logger.info("map " + scopes + " generated from " + scopeString);

        return scopes;
    }

}
