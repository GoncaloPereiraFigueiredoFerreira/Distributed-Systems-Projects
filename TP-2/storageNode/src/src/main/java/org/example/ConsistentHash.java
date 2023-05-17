package org.example;

import java.math.BigInteger;
import java.security.NoSuchAlgorithmException;
import java.util.Collection;
import java.util.List;
import java.util.SortedMap;
import java.util.TreeMap;

public class ConsistentHash {
    private final HashingAlgorithm hashingAlgorithm;
    private final int numberOfReplicas;
    private final SortedMap<BigInteger, Integer> circle = new TreeMap<>();

    public ConsistentHash(int hashingAlgorithm, int numberOfReplicas,
                          List<String> nodes) throws NoSuchAlgorithmException {
        this.hashingAlgorithm = new HashingAlgorithm(hashingAlgorithm);
        this.numberOfReplicas = numberOfReplicas;

        for (String node : nodes) {
            add(node);
        }
    }

    public void add(Integer node) {
        for (int i = 0; i <numberOfReplicas; i++) {
            circle.put(hashingAlgorithm.hash(node + i), node);
        }
    }

    public void remove(Integer node) {
        for (int i = 0; i <numberOfReplicas; i++) {
            circle.remove(hashingAlgorithm.hash(node + i));
        }
    }

    public Integer getNode(Integer key) {
        if (circle.isEmpty()) {
            return null;
        }
        BigInteger hash = hashingAlgorithm.hash(key);
        if (!circle.containsKey(hash)) {
            SortedMap<BigInteger, Integer> tailMap = circle.tailMap(hash);
            hash = tailMap.isEmpty() ? circle.firstKey() : tailMap.firstKey();
        }
        return circle.get(hash);
    }
}