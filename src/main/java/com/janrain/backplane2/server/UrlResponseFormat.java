package com.janrain.backplane2.server;

import java.io.UnsupportedEncodingException;
import java.net.URLEncoder;
import java.nio.charset.Charset;
import java.util.Map;

/**
 * Class for building an OAuth2 URL response with parameters sent out in either query or fragment
 *
 * @author Johnny Bufu
 */
public enum UrlResponseFormat {

    QUERY {
        @Override
        public String encode(String redirectUrl, Map<String, String> params) throws ValidationException {
            OAuth2.validateRedirectUri(redirectUrl); // no fragment
            StringBuilder result = new StringBuilder(redirectUrl);
            stitchParams(result, params, redirectUrl.contains("?") ? "&" : "?");
            return result.toString();
        }},

    FRAGMENT {
        @Override
        public String encode(String redirectUrl, Map<String, String> params) throws ValidationException {
            OAuth2.validateRedirectUri(redirectUrl); // no fragment
            StringBuilder result = new StringBuilder(redirectUrl);
            stitchParams(result, params, "#");
            return result.toString();
        }};

    public abstract String encode(String redirectUri, Map<String,String> params) throws ValidationException;

    // - PRIVATE

    private static final Charset UTF8 = Charset.forName("UTF-8");


    private static void stitchParams(StringBuilder result, Map<String,String> params, String firstChar) {
        boolean firstParam = true;
        for(Map.Entry<String,String> param : params.entrySet()) {
            result.append( firstParam ? firstChar : "&");
            firstParam = false;
            try {
                result.append(URLEncoder.encode(param.getKey(), UTF8.name()));
                result.append("=");
                result.append(URLEncoder.encode(param.getValue(), UTF8.name()));
            } catch (UnsupportedEncodingException e) {
                // do nothing, Utf8StringUtils.UTF8 would have crashed at deploy-time
            }
        }
    }
}
