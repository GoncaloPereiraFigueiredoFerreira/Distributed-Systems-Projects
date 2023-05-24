package org.example;

import java.security.NoSuchAlgorithmException;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

public class DataStorage {
    private ConsistentHash consistentHash;
    private Map<String,String> map;
    public DataStorage(int hashingAlgorithm, int numberOfReplicas) throws NoSuchAlgorithmException {
        this.consistentHash = new ConsistentHash(hashingAlgorithm,numberOfReplicas);
        this.map = new TreeMap<>();
    }

    public void insertKey(String chave, String value,int versao){
        this.map.put(chave,value); //TODO CRDTS
    }

    public String getKey(String chave,int versao){
        return this.map.get(chave); //TODO CRDTS
    }

    public List<Integer> addNode(String serverAddress){
        return this.consistentHash.addNode(serverAddress);
    }
}
