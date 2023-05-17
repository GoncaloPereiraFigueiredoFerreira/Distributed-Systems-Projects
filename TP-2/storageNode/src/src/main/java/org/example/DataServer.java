package org.example;

import java.security.KeyPair;
import java.security.NoSuchAlgorithmException;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.TreeMap;

public class DataServer {
    private ConsistentHash consistentHash;
    private Map<String,String> map;
    public DataServer(int hashingAlgorithm, int numberOfReplicas,
                      List<String> nodes) throws NoSuchAlgorithmException {
        this.consistentHash = new ConsistentHash(1,2,List.of("server1","server2"));
        this.map = new TreeMap<>();
    }

    public void insertKey(String chave, String value){
        this.map.put(chave,value); //TODO CRDTS
    }

    public String getKey(String chave){
        return this.map.get(chave); //TODO CRDTS
    }

    public void addServer(Integer serverKey){
        this.consistentHash.add(serverKey);
    }

    public void removeServer(Integer serverKey){
        this.consistentHash.remove(serverKey);
    }
}
