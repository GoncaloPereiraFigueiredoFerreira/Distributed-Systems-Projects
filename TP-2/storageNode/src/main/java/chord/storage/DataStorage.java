package chord.storage;


import chord.hashing.HashingAlgorithm;

import java.security.NoSuchAlgorithmException;
import java.util.*;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

public class DataStorage {
    private final Map<String,List<Version>> map;
    private final HashingAlgorithm hashingAlgorithm;
    ReadWriteLock readWriteLock;
    int nodeId;
    public DataStorage(HashingAlgorithm hashingAlgorithm,int nodeId){
        this.hashingAlgorithm = hashingAlgorithm;
        this.readWriteLock = new ReentrantReadWriteLock();
        this.map = new HashMap<>();
        this.nodeId=nodeId;
    }

    public Map<String, List<Version>> getMap() {
        return map;
    }

    public void insertKey(List<Dependencie> dependencies, String key, String value){
        readWriteLock.writeLock().lock();
        try {
            Version newVersion = new Version(dependencies,value);
            if(!map.containsKey(key)){
                map.put(key,new ArrayList<>());
            }
            List<Version> versionList = new ArrayList<>(map.get(key));
            versionList.add(newVersion);
            map.put(key,versionList);
        }finally {
            readWriteLock.writeLock().unlock();
        }
    }

    public int insertKey(String key, Version version){
        readWriteLock.writeLock().lock();
        try {
            if(!map.containsKey(key)){
                map.put(key,new ArrayList<>());
            }
            List<Version> versionList = new ArrayList<>(map.get(key));
            versionList.add(version);
            map.put(key,versionList);
            return versionList.size()-1;
        }finally {
            readWriteLock.writeLock().unlock();
        }
    }

    public Version getKey(String key,int version){
        readWriteLock.readLock().lock();
        try {
            if(map.containsKey(key)){
                List<Version> versionList = this.map.get(key);
                if(versionList.size()>version)
                    return versionList.get(version);
            }
            return null;
        }
        finally {
            readWriteLock.readLock().unlock();
        }
    }

    public Version getKey(String key){
        readWriteLock.readLock().lock();
        try {
            if(map.containsKey(key)){
                List<Version> versionList = this.map.get(key);
                return versionList.get(versionList.size()-1);
            }
            return null;
        }
        finally {
            readWriteLock.readLock().unlock();
        }
    }

    public int getLastKeyVersion(String key){
        readWriteLock.readLock().lock();
        try {
            if(map.containsKey(key)){
                List<Version> versionList = this.map.get(key);
                return versionList.size()-1;
            }
            return -1;
        }
        finally {
            readWriteLock.readLock().unlock();
        }
    }

    public Map<String,List<Version>> moveKeys(int id) {
        readWriteLock.writeLock().lock();
        try {
            Map<String,List<Version>> keysToChange = new HashMap<>();
            for(Map.Entry<String,List<Version>> entry:map.entrySet()){
                if(isInRange(id,hashingAlgorithm.hash(entry.getKey()),nodeId)){
                    keysToChange.put(entry.getKey(),entry.getValue());
                    map.remove(entry.getKey());
                }
            }
            return keysToChange;
        }finally {
            readWriteLock.writeLock().unlock();
        }
    }

    public static String keysToString(Map<String,List<Version>> map) {
        StringBuilder stringBuilder = new StringBuilder();
        map.forEach((key, value) -> {
            stringBuilder.append(key).append("||");
            value.forEach(version -> stringBuilder.append(version.toString()).append("||"));
            stringBuilder.append("|");
        });
        return stringBuilder.toString();
    }
    public void insertFromString(String value){
        //"key1||version||version|||key2||version|||"
        if(!Objects.equals(value, "")) {
            String[] keySets = value.split("\\|{3}");
            for (String entry : keySets) {
                String[] vals = entry.split("\\|{2}");
                String key = vals[0];
                List<Version> versions = Arrays.stream(Arrays.copyOfRange(vals, 1, vals.length))
                        .map(e -> Version.fromStrings(e.split("\\|"))).toList();
                map.put(key, versions);
            }
        }
    }

    public static boolean isInRange(int key, int a, int b) {
        if (a > b) {
            return a < key || b > key;
        } else if (a < b) {
            return a < key && b > key;
        } else {
            return a != key;
        }
    }
}
