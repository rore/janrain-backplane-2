package com.janrain.backplane2.server;

import org.apache.commons.lang.StringUtils;
import org.apache.log4j.Logger;
import org.jetbrains.annotations.Nullable;

import java.util.Collection;
import java.util.EnumSet;
import java.util.HashMap;
import java.util.Map;

import static com.janrain.oauth2.OAuth2.*;

/**
 * OAuth2 grant types used by Backplane
 */
public enum GrantType {

    REFRESH_ANONYMOUS("AR"),
    ANONYMOUS("AA", REFRESH_ANONYMOUS),

    REFRESH_PRIVILEGED( "PR", OAUTH2_TOKEN_GRANT_TYPE_REFRESH_TOKEN ),
    AUTHORIZATION_CODE( "PA", REFRESH_PRIVILEGED, OAUTH2_TOKEN_GRANT_TYPE_AUTH_CODE ),
    CLIENT_CREDENTIALS( "PC", REFRESH_PRIVILEGED, OAUTH2_TOKEN_GRANT_TYPE_CLIENT_CREDENTIALS );

    public String getTokenPrefix() {
        return tokenPrefix;
    }

    public String getOauthType() {
        return oauthType;
    }

    public boolean isPrivileged() {
        return this.oauthType != null;
    }

    public GrantType getRefreshType() {
        return refreshType;
    }

    public boolean isRefreshable() {
        return refreshType != null;
    }

    public boolean isRefresh() {
        return refreshType == null;
    }

    public int getTokenExpiresSecondsDefault() {
        int seconds = isPrivileged() ? TOKEN_PRIVILEGED_EXPIRES_SECONDS : TOKEN_ANONYMOUS_EXPIRES_SECONDS;
        return isRefresh() ? 2 * seconds : seconds;
    }

    public Collection<TokenSource> getTokenAllowedSources() {
        return isPrivileged() ? EnumSet.of(TokenSource.AUTHHEADER) : EnumSet.allOf(TokenSource.class);
    }

    /**
     * @return the GrantType recognized based on token prefix, or null if not recognized
     */
    public static GrantType fromTokenString(String tokenString) {
        if ( StringUtils.isEmpty(tokenString) || tokenString.length() < TOKEN_PREFIX_LENGTH ) {
            return null;
        }
        return mapByTokenPrefix.get(tokenString.substring(0, TOKEN_PREFIX_LENGTH));
    }

    public static GrantType fromOauthType(String oauthGrantType) {
        return mapByOauthType.get(oauthGrantType);
    }

    // - PRIVATE

    private static final Logger logger = Logger.getLogger(GrantType.class);

    public static final int TOKEN_PRIVILEGED_EXPIRES_SECONDS = 31536000;  // 1 year
    public static final int TOKEN_PRIVILEGED_TEST_EXPIRES_SECONDS = 600;  // 10 minutes

    public static final int TOKEN_ANONYMOUS_EXPIRES_SECONDS = 604800;     // 1 week
    public static final int TOKEN_ANONYMOUS_TEST_EXPIRES_SECONDS = 600;   // 10 minutes

    private static final int TOKEN_PREFIX_LENGTH = 2;

    private static final Map<String,GrantType> mapByOauthType = new HashMap<String, GrantType>();
    static {
        for (GrantType t : values()) {
            if (t.oauthType != null) {
                mapByOauthType.put(t.oauthType, t);
            }
        }
    }

    private static final Map<String,GrantType> mapByTokenPrefix = new HashMap<String, GrantType>();
    static {
        for (GrantType t : values()) {
            mapByTokenPrefix.put(t.tokenPrefix, t);
        }
    }

    private final String tokenPrefix;
    private final String oauthType;
    private final GrantType refreshType;

    private GrantType(String tokenPrefix) {
        this(tokenPrefix, null, null);
    }

    private GrantType(String tokenPrefix, GrantType refreshType) {
        this(tokenPrefix, refreshType, null);
    }

    private GrantType(String tokenPrefix, @Nullable String oauthType) {
        this(tokenPrefix, null, oauthType);
    }

    private GrantType(String tokenPrefix, @Nullable GrantType refreshType, @Nullable String oauthType) {
        this.tokenPrefix = tokenPrefix;
        this.oauthType = oauthType;
        this.refreshType = refreshType;
    }
}
