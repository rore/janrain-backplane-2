package com.janrain.backplane2.server.dao.redis;

import com.janrain.backplane2.server.BackplaneServerException;
import com.janrain.backplane2.server.dao.AuthorizationRequestDAO;
import com.janrain.oauth2.AuthorizationRequest;
import com.janrain.redis.Redis;
import org.apache.commons.lang.NotImplementedException;
import org.apache.commons.lang.SerializationUtils;

import java.util.List;

/**
 * @author Tom Raney
 */
public class RedisAuthorizationRequestDAO implements AuthorizationRequestDAO {

    public static byte[] getKey(String id) {
        return new String("v2_auth_request_" + id).getBytes();
    }

    @Override
    public AuthorizationRequest get(String id) throws BackplaneServerException {
        byte[] bytes = Redis.getInstance().get(getKey(id));
        if (bytes != null) {
            return (AuthorizationRequest) SerializationUtils.deserialize(bytes);
        } else {
            return null;
        }
    }

    @Override
    public List<AuthorizationRequest> getAll() throws BackplaneServerException {
        throw new NotImplementedException();
    }

    @Override
    public void persist(AuthorizationRequest authorizationRequest) throws BackplaneServerException {
        Redis.getInstance().set(getKey(authorizationRequest.getIdValue()), SerializationUtils.serialize(authorizationRequest));
    }

    @Override
    public void delete(String id) throws BackplaneServerException {
        Redis.getInstance().del(getKey(id));
    }

    @Override
    public AuthorizationRequest retrieveAuthorizationRequest(String cookie) throws BackplaneServerException {
        throw new NotImplementedException();
    }

    @Override
    public void deleteExpiredAuthorizationRequests() {
        throw new NotImplementedException();
    }
}
