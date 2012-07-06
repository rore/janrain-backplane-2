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

package com.janrain.locking;

import org.apache.zookeeper.*;
import org.apache.zookeeper.data.Stat;

import java.io.IOException;
import java.util.List;

/**
 * @author Tom Raney
 */
public class ZooKeeper implements Watcher {

    private org.apache.zookeeper.ZooKeeper zooKeeper;
    private static ZooKeeper instance;

    private ZooKeeper() {
        try {
            zooKeeper = new org.apache.zookeeper.ZooKeeper("localhost:2181", 3000, this);
            this.addNode("/messages", null);
            this.addNode("/channels", null);
            this.addNode("/buses", null);


        } catch (IOException e) {
            System.out.println("ZooKeeper failed! " + e.getMessage());
        }

    }

    public static synchronized ZooKeeper getInstance() {
        if (instance == null) {
            instance = new ZooKeeper();
        }
        return instance;
    }

    public String addNode(String path, byte[] data) {
        try {
            return zooKeeper.create(path, data, ZooDefs.Ids.OPEN_ACL_UNSAFE, CreateMode.PERSISTENT);
        } catch (KeeperException e) {
            // locked?
            System.out.println("Could not write node: " + e.getLocalizedMessage());
        } catch (InterruptedException e) {
            System.out.println("Could not write node: " + e.getLocalizedMessage());
        }
        return null;

    }

    public Stat setData(String path, byte[] data, int version) {
        try {
            return zooKeeper.setData(path, data, version);
        } catch (InterruptedException e) {
            System.out.println("Could not update node: " + e.getLocalizedMessage());
        } catch (KeeperException e) {
            System.out.println("Could not update node: " + e.getLocalizedMessage());
        }
        return null;
    }

    public Stat exists(String path, boolean watch) {
        try {
            return zooKeeper.exists(path, watch);
        } catch (InterruptedException e) {
            System.out.println("exists check failed: " + e.getLocalizedMessage());
        } catch (KeeperException e) {
            System.out.println("exists check failed: " + e.getLocalizedMessage());
        }
        return null;
    }

    public byte[] getData(String path, Stat stat) {
        try {
            return zooKeeper.getData(path, this, stat);
        } catch (KeeperException e) {
            System.out.println("Could not read node: " + e.getLocalizedMessage());
        } catch (InterruptedException e) {
            System.out.println("Could not read node: " + e.getLocalizedMessage());
        }
        return null;
    }

    public List<String> getChildren(String path, boolean watch) {
        try {
            return zooKeeper.getChildren(path, watch);
        } catch (InterruptedException e) {
            System.out.println("Could not getChildren: " + e.getLocalizedMessage());
        } catch (KeeperException e) {
            System.out.println("Could not getChildren: " + e.getLocalizedMessage());
        }
        return null;

    }

    public String getDistributedLock(String mutex) {
        synchronized (mutex.intern()) {
            int wait = 4;
            while (wait-- > 0) {
                try {
                    String lock = zooKeeper.create("/lock_" + mutex, new byte[0], ZooDefs.Ids.OPEN_ACL_UNSAFE, CreateMode.EPHEMERAL);
                    System.out.println("lock: " + lock + " created");
                    return lock;
                } catch (KeeperException e) {
                    // we get here if lock failed
                    System.out.println("failed to get lock: " + e.getMessage());
                } catch (InterruptedException e) {

                }
                try {
                    Thread.sleep(250);
                } catch (InterruptedException e) {

                }
            }
            System.out.println("aborted without lock");
            return null;
        }
    }

    public void releaseLock(String lock) {
        try {
            zooKeeper.delete(lock, -1);
            System.out.println("lock: " + lock + " released");
        } catch (InterruptedException e) {

        } catch (KeeperException e) {

        }
    }

    @Override
    public void process(WatchedEvent watchedEvent) {
        System.out.println("received watchedEvent" + watchedEvent.getPath());

    }
}
