package com.janrain.backplane2.server.dao;

import com.janrain.backplane2.server.BackplaneServerException;
import com.janrain.backplane2.server.config.User;
import com.janrain.redis.Redis;
import org.apache.commons.lang.NotImplementedException;
import org.apache.commons.lang.SerializationUtils;
import org.apache.log4j.Logger;

import java.util.List;

/**
 * @author Tom Raney
 */
public interface AdminDAO extends DAO<User> {


}

