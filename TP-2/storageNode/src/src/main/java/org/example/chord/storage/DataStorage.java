package org.example.chord.storage;

import org.example.ConsistentHash;
import org.example.HashingAlgorithm;

import java.security.NoSuchAlgorithmException;
import java.util.*;
import java.util.concurrent.locks.ReadWriteLock;
import java.util.concurrent.locks.ReentrantReadWriteLock;

public class DataStorage {
    private final Map<String,List<Version>> map;
    private final HashingAlgorithm hashingAlgorithm;
    ReadWriteLock readWriteLock;
    public DataStorage(HashingAlgorithm hashingAlgorithm){
        this.hashingAlgorithm = hashingAlgorithm;
        this.readWriteLock = new ReentrantReadWriteLock();
        this.map = new HashMap<>();
    }

    public void insertKey(List<Dependencie> dependencies,String key, String value){
        try {
            readWriteLock.writeLock().lock();
            Version newVersion = new Version(dependencies,value);
            if(!map.containsKey(key)){
                map.put(key,new ArrayList<>());
            }
            List<Version> versionList = map.get(key);
            versionList.add(newVersion);
        }finally {
            readWriteLock.writeLock().unlock();
        }
    }

    public void insertKey(String key, Version version){
        try {
            readWriteLock.writeLock().lock();
            if(!map.containsKey(key)){
                map.put(key,new ArrayList<>());
            }
            List<Version> versionList = map.get(key);
            versionList.add(version);
        }finally {
            readWriteLock.writeLock().unlock();
        }
    }

    public Version getKey(String key,int version){
        try {
            readWriteLock.readLock().lock();
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
        try {
            readWriteLock.readLock().lock();
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
        try {
            readWriteLock.readLock().lock();
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
}
