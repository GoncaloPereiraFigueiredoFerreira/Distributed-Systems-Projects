package chord.hashing;

import java.security.NoSuchAlgorithmException;
import java.util.*;

public class ConsistentHash {
    private final HashingAlgorithm hashingAlgorithm;
    private final int numberOfReplicas;
    private final SortedMap<Integer, String> circle = new TreeMap<>();

    public ConsistentHash(int hashingAlgorithm, int numberOfReplicas) throws NoSuchAlgorithmException {
        this.hashingAlgorithm = new HashingAlgorithm(hashingAlgorithm);
        this.numberOfReplicas = numberOfReplicas;
    }

    public List<Integer> addNode(String nodeAddress) {
        List<Integer> nodes = new ArrayList<>();
        for (int i = 0; i <numberOfReplicas; i++) {
            Integer key = hashingAlgorithm.hash(nodeAddress + i);
            nodes.add(key);
            circle.put(key,nodeAddress);
        }
        return nodes;
    }

    public String getNode(Integer key) {
        if (circle.isEmpty()) {
            return null;
        }
        Integer hash = hashingAlgorithm.hash(key);
        if (!circle.containsKey(hash)) {
            SortedMap<Integer, String> tailMap = circle.tailMap(hash);
            hash = tailMap.isEmpty() ? circle.firstKey() : tailMap.firstKey();
        }
        return circle.get(hash);
    }
}