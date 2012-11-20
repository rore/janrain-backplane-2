package com.janrain.backplane2.server;

import com.janrain.backplane.common.User;

/**
 * @author Johnny Bufu
 */
public class BusOwner extends User {

    public BusOwner() { }

    public BusOwner(String name, String secret) {
        super(name, secret);
    }
}
