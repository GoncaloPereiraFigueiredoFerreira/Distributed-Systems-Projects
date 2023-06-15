package chord;

import org.zeromq.ZContext;
import java.util.*;
import java.util.concurrent.locks.ReentrantReadWriteLock;


public class SocketCache {
    private HashMap<String, LockedSocket> cache;
    private final ZContext context;
    private final ReentrantReadWriteLock readWriteLock;

    public SocketCache(ZContext context) {
        cache = new HashMap<>();
        this.context = context;
        this.readWriteLock= new ReentrantReadWriteLock();
    }

    public String sendAndReceive(String destiny, String nodeId, String message) {
        readWriteLock.readLock().lock();
        try {
            LockedSocket lockedSocket = cache.get(destiny);
            if (lockedSocket == null) {
                readWriteLock.readLock().unlock();
                readWriteLock.writeLock().lock();
                try {
                    lockedSocket = cache.get(destiny);
                    if (lockedSocket == null) {
                        lockedSocket = new LockedSocket(context, destiny);
                        cache.put(destiny, lockedSocket);
                    }
                } finally {
                    readWriteLock.writeLock().unlock();
                    readWriteLock.readLock().lock();
                }
            }
            return lockedSocket.sendAndReceive(nodeId, message);
        } finally {
            readWriteLock.readLock().unlock();
        }
    }


    public void removeIfNotContains(List<String> keys) {
        List<String> keysToRemove = new ArrayList<>();

        readWriteLock.readLock().lock();
        try {
            for (Map.Entry<String, LockedSocket> entry : cache.entrySet()) {
                String key = entry.getKey();
                LockedSocket value = entry.getValue();
                if (!keys.contains(key)) {
                    value.destroy(context);
                    keysToRemove.add(key);
                }
            }
        } finally {
            readWriteLock.readLock().unlock();
        }

        if(keysToRemove.size()>0){
            readWriteLock.writeLock().lock();
            try{
                for (String key : keysToRemove) {
                    cache.remove(key);
                }
            } finally {
                readWriteLock.writeLock().unlock();
            }
        }
    }


    public String printOrder() {
        readWriteLock.readLock().lock();
        try {
            StringBuilder sb = new StringBuilder();
            for (String key : cache.keySet()) {
                sb.append(key).append(" -> ");
            }
            sb.append("\n");
            return sb.toString();
        } finally {
            readWriteLock.readLock().unlock();
        }
    }
}
