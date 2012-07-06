package com.janrain.backplane2.server.dao.redis;

import com.janrain.backplane2.server.AuthSession;
import com.janrain.backplane2.server.BackplaneServerException;
import com.janrain.backplane2.server.dao.AuthSessionDAO;
import com.janrain.redis.Redis;
import org.apache.commons.lang.NotImplementedException;
import org.apache.commons.lang.SerializationUtils;

import java.util.List;

/**
 * @author Tom Raney
 */
public class RedisAuthSessionDAO implements AuthSessionDAO {

    public static byte[] getKey(String id) {
        return ("v2_auth_session_" + id).getBytes();
    }

    @Override
    public AuthSession get(String id) throws BackplaneServerException {
        byte[] bytes = Redis.getInstance().get(getKey(id));
        if (bytes != null) {
            return (AuthSession) SerializationUtils.deserialize(bytes);
        } else {
            return null;
        }
    }

    @Override
    public List<AuthSession> getAll() throws BackplaneServerException {
        throw new NotImplementedException();
    }

    @Override
    public void persist(AuthSession authSession) throws BackplaneServerException {
        Redis.getInstance().set(getKey(authSession.getIdValue()), SerializationUtils.serialize(authSession));
    }

    @Override
    public void delete(String id) throws BackplaneServerException {
        throw new NotImplementedException();
    }

    @Override
    public void deleteExpiredAuthSessions() {
        throw new NotImplementedException();
    }
}
