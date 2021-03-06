package com.janrain.backplane2.server.dao.redis;

import com.janrain.backplane2.server.BackplaneServerException;
import com.janrain.backplane2.server.dao.AuthorizationDecisionKeyDAO;
import com.janrain.oauth2.AuthorizationDecisionKey;
import com.janrain.redis.Redis;
import org.apache.commons.lang.NotImplementedException;
import org.apache.commons.lang.SerializationUtils;

import java.util.List;

/**
 * @author Tom Raney
 */
public class RedisAuthorizationDecisionKeyDAO implements AuthorizationDecisionKeyDAO {

    public static byte[] getKey(String id) {
        return ("v2_auth_decision_" + id).getBytes();
    }

    @Override
    public AuthorizationDecisionKey get(String id) throws BackplaneServerException {
        byte[] bytes = Redis.getInstance().get(getKey(id));
        if (bytes != null) {
            return (AuthorizationDecisionKey) SerializationUtils.deserialize(bytes);
        } else {
            return null;
        }
    }

    @Override
    public List<AuthorizationDecisionKey> getAll() throws BackplaneServerException {
        throw new NotImplementedException();
    }

    @Override
    public void persist(AuthorizationDecisionKey authorizationDecisionKey) throws BackplaneServerException {
        Redis.getInstance().set(getKey(authorizationDecisionKey.getIdValue()), SerializationUtils.serialize(authorizationDecisionKey));
    }

    @Override
    public void delete(String id) throws BackplaneServerException {
        throw new NotImplementedException();
    }

    @Override
    public void deleteExpiredAuthorizationDecisionKeys() {
        throw new NotImplementedException();
    }
}
