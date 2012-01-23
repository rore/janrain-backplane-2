package com.janrain.backplane.server;

import com.janrain.commons.supersimpledb.message.MessageField;
import com.janrain.crypto.ChannelUtil;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;

import java.util.*;

/**
 * @author Tom Raney
 */

public class Token extends Base {

    private static final int CHANNEL_NAME_LENGTH = 32;
    public static final int TOKEN_LENGTH = 20;
    public static final int EXPIRES_SECONDS = 3600;
    public static final String ANONYMOUS = "anonymous";


    public static enum TYPE {REGULAR_TOKEN, PRIVILEGED_TOKEN}

    /**
     * Empty default constructor for AWS to use.
     * Don't call directly.
     */
    public Token() {};

    /**
     * Token constructor
     * @param tokenString required
     * @param accessType  REGULAR_TOKEN or PRIVILEGED_TOKEN
     * @param buses       optional
     * @param scopeString optional
     * @param expires     if null, token does not expire
     * @throws BackplaneServerException
     */
    Token(String tokenString, TYPE accessType, String buses, String scopeString, Date expires) throws BackplaneServerException {
        super(tokenString,buses,expires);

        put(TokenField.TYPE.getFieldName(), accessType.name());

        if (accessType == TYPE.REGULAR_TOKEN) {
            String channel = ChannelUtil.randomString(CHANNEL_NAME_LENGTH);
            put(TokenField.CHANNEL.getFieldName(), channel);
            // set the scope string to include this new channel
            if (StringUtils.isEmpty(scopeString)) {
                scopeString = "channel:" + channel;
            }  else {
                scopeString += " channel:" + channel;
            }
        }

        if (StringUtils.isNotEmpty(scopeString)) {
            put(TokenField.SCOPE.getFieldName(), scopeString);
        }

        if (accessType == TYPE.PRIVILEGED_TOKEN) {
            if (new Scope(scopeString).getBusesInScope().isEmpty()) {
                // if a privileged user has requested a token without specifying a bus in the scope, copy
                // over all authorized buses from the set of authorized buses

                if (StringUtils.isBlank(scopeString)) {
                    scopeString = "";
                }

                scopeString = getEncodedBusesAsString() + " " + scopeString;
                this.setScopeString(scopeString);
            }

            if (!isAllowedBuses(new Scope(this.getScopeString()).getBusesInScope())) {
                throw new BackplaneServerException("Scope request not allowed");
            }
        }
    }

    public Token(TYPE accessType, String buses, String scope, Date expires) throws BackplaneServerException {
        this(ChannelUtil.randomString(TOKEN_LENGTH), accessType, buses, scope, expires);
    }

    public Token(Grant grant, String scope) throws BackplaneServerException {
        this(ChannelUtil.randomString(TOKEN_LENGTH), TYPE.PRIVILEGED_TOKEN, grant.getBusesAsString(), scope, null);
        this.addGrant(grant);
    }

    public Token(List<Grant> grants, String scope) throws BackplaneServerException {
        this(ChannelUtil.randomString(TOKEN_LENGTH), TYPE.PRIVILEGED_TOKEN, Grant.getBusesAsString(grants), scope, null);
        this.setGrants(grants);
    }

    public String getChannelName() {
        return this.get(TokenField.CHANNEL);
    }

    public String getTokenType() {
        return "Bearer";
    }

    public TYPE getAccessType() {
        return TYPE.valueOf(this.get(TokenField.TYPE));
    }

    public Scope getScope() throws BackplaneServerException {
        return new Scope(this.get(TokenField.SCOPE));
    }

    public boolean isPrivileged() {
        return getAccessType() == TYPE.PRIVILEGED_TOKEN;
    }

    public String getScopeString() {
        return get(TokenField.SCOPE);
    }

    public void setScopeString(String scopeString) {
        scopeString = scopeString.trim();
        logger.debug("new scope string: '" + scopeString + "'");
        put(TokenField.SCOPE.getFieldName(), scopeString);
    }

    public List<Grant> getGrants() {
        return Collections.unmodifiableList(this.sourceGrants);
    }

    public void setGrants(List<Grant> grants) {
        this.sourceGrants = new ArrayList<Grant>(grants);
    }


    public static enum TokenField implements MessageField {
        TYPE("type", true),
        SCOPE("scope", false),
        CHANNEL("channel", false);


        @Override
        public String getFieldName() {
            return fieldName;
        }

        @Override
        public boolean isRequired() {
            return required;
        }

        @Override
        public void validate(String value) throws RuntimeException {
            if (isRequired()) validateNotNull(getFieldName(), value);
        }

        // - PRIVATE

        private String fieldName;
        private boolean required = true;

        private TokenField(String fieldName) {
            this(fieldName, true);
        }

        private TokenField(String fieldName, boolean required) {
            this.fieldName = fieldName;
            this.required = required;
        }
    }

    private static final Logger logger = Logger.getLogger(Token.class);

    private ArrayList<Grant> sourceGrants = new ArrayList<Grant>();

    private void addGrant(Grant grant) {
        this.sourceGrants.add(grant);
    }

}
