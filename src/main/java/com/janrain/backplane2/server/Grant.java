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

package com.janrain.backplane2.server;

import com.janrain.backplane2.server.config.Backplane2Config;
import com.janrain.commons.supersimpledb.message.MessageField;
import com.janrain.crypto.ChannelUtil;
import org.apache.commons.lang.NotImplementedException;
import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.jetbrains.annotations.NotNull;

import java.text.ParseException;
import java.util.*;

/**
 * @author Tom Raney
 */
public class Grant extends Base {

    public static final int CODE_LENGTH = 20;
    public static final int CODE_EXPIRATION = 3600;

    public static String getBusesAsString(List<Grant> grants) {
        StringBuilder sb = new StringBuilder();
        for (Grant grant: grants) {
            sb.append(grant.getBusesAsString() + " ");
        }
        return sb.toString().trim();
    }


    /**
     * Empty default constructor for AWS to use.
     * Don't call directly.
     */
    public Grant() {};

    Grant(String code, String busOwnerId, String clientId, String buses, Date expires) {


        super(code, buses, expires);

        assert(StringUtils.isNotEmpty(clientId));
        assert(StringUtils.isNotEmpty(busOwnerId));
        assert(StringUtils.isNotEmpty(buses));

        put(GrantField.ISSUED_TO_CLIENT_ID.getFieldName(), clientId);
        put(GrantField.ISSUED_BY_USER_ID.getFieldName(), busOwnerId);

        logger.info("Grant declared by bus owner " + busOwnerId + " for client " + clientId + " with buses: " + buses);
    }

    public Grant(String busOwnerId, String clientId, String buses) {
        this(ChannelUtil.randomString(CODE_LENGTH), busOwnerId, clientId, buses, null);
    }

    public Grant(String busOwnerId, String clientId, String buses, Date expires) {
        this(ChannelUtil.randomString(CODE_LENGTH), busOwnerId, clientId, buses, expires);
    }

    public String getGrantClientId() {
        return get(GrantField.ISSUED_TO_CLIENT_ID.getFieldName());
    }

    public void revokeAuth() {
        throw new NotImplementedException();
    }

    public Date getCodeExpiresDate() {

        String expiresString = get(GrantField.DATE_CODE_EXPIRES);

        if (expiresString == null) {
            return null;
        }

        Date expires = null;
        try {
            expires = Backplane2Config.ISO8601.parse(expiresString);
        } catch (ParseException e) {
            return null;
        }
        return expires;
    }

    public Date getCodeIssuedDate() {

        String issuedString = get(GrantField.DATE_CODE_ISSUED);

        if (issuedString == null) {
            return null;
        }

        Date issued = null;
        try {
            issued = Backplane2Config.ISO8601.parse(issuedString);
        } catch (ParseException e) {
            return null;
        }
        return issued;
    }

    public Date getCodeUsedDate() {

        String usedString = get(GrantField.DATE_CODE_USED);

        if (usedString == null) {
            return null;
        }

        Date issued = null;
        try {
            issued = Backplane2Config.ISO8601.parse(usedString);
        } catch (ParseException e) {
            return null;
        }
        return issued;
    }

    public void setCodeUsedNow() {
        put(GrantField.DATE_CODE_USED.getFieldName(), Backplane2Config.ISO8601.format(new Date()));
    }

    public void setCodeIssuedNow() {
        put(GrantField.DATE_CODE_ISSUED.getFieldName(), Backplane2Config.ISO8601.format(new Date()));
    }

    public boolean isCodeIssued() {
        return getCodeIssuedDate() != null;
    }

    public boolean isCodeUsed() {
        return getCodeUsedDate() != null;
    }

    public boolean isCodeExpired() {
        Date expires = getCodeExpiresDate();
        if (expires != null && new Date().getTime() > expires.getTime()) {
            return true;
        }
        return false;
    }

    public void setCodeExpirationDate(Date expirationDate) {
        put(GrantField.DATE_CODE_EXPIRES.getFieldName(), Backplane2Config.ISO8601.format(expirationDate));
    }

    public void setCodeExpirationDefault() {
        this.setCodeExpirationDate(new Date(new Date().getTime() + CODE_EXPIRATION*1000L ));
    }

    private String getIssuedTokenIdsAsString() {
        return get(GrantField.ISSUED_TOKEN_IDS.getFieldName());
    }

    /**
     * Retrieve list of token Ids issued against this grant
     * @return a non-null but possibly empty list
     */
    public @NotNull
    List<String> getIssuedTokenIds() {
        String ids = getIssuedTokenIdsAsString();
        if (StringUtils.isEmpty(ids)) {
            return new ArrayList<String>();
        }

        String[] tokens = ids.split(" ");
        return Arrays.asList(tokens);
    }

    public boolean isIssuedToken(String tokenId) {
        return getIssuedTokenIds().contains(tokenId);
    }

    public void addIssuedTokenId(@NotNull String tokenId) {
        List<String> tokenIds = getIssuedTokenIds();
        if (tokenIds.contains(tokenId)) {
            // if it exists, no-op
            return;
        }
        tokenIds.add(tokenId);
        setIssuedTokens(tokenIds);
    }

    public void setIssuedTokens(List<String> tokenIds) {
        put(GrantField.ISSUED_TOKEN_IDS.getFieldName(),
                org.springframework.util.StringUtils.collectionToDelimitedString(tokenIds, " "));
    }

    public boolean removeIssuedTokenId(@NotNull String tokenId) {
        List<String> tokenIds = getIssuedTokenIds();
        if (!tokenIds.remove(tokenId)) {
            return false;
        }
        setIssuedTokens(tokenIds);
        return true;
    }


    public static enum GrantField implements MessageField {

        // - PUBLIC

        ISSUED_BY_USER_ID("issued_by_user"),
        ISSUED_TO_CLIENT_ID("issued_to_client"),
        DATE_CODE_ISSUED("date_code_issued"),
        DATE_CODE_EXPIRES("date_code_expires"),
        DATE_CODE_USED("date_code_used"),
        ISSUED_TOKEN_IDS("issued_token_ids", false);

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

        private GrantField(String fieldName) {
            this(fieldName, true);
        }

        private GrantField(String fieldName, boolean required) {
            this.fieldName = fieldName;
            this.required = required;
        }
    }

    private static final Logger logger = Logger.getLogger(Access.class);

}
