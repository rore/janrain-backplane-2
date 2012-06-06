package com.janrain.locking;

import org.apache.zookeeper.*;

import java.io.IOException;

/**
 * @author Tom Raney
 */
public class DistributedLockingManager implements Watcher {

    private ZooKeeper zooKeeper;
    private static DistributedLockingManager instance;

    private DistributedLockingManager() {
        try {
            zooKeeper = new ZooKeeper("localhost:2181", 3000, this);
        } catch (IOException e) {
            System.out.println("ZooKeeper failed! " + e.getMessage());
        }

    }

    public static synchronized DistributedLockingManager getInstance() {
        if (instance == null) {
            instance = new DistributedLockingManager();
        }
        return instance;
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

    }
}
