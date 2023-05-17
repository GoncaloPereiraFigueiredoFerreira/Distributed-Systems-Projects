package org.example.chord;

import org.example.ConsistentHash;

import java.util.HashMap;
import java.util.List;

public class Node {
    private int id;
    private final HashMap<Integer,Finger> fingerTable;
    private ConsistentHash consistentHash;
    private final int m;
    int successor;

    public Node(int id, int m, ConsistentHash consistentHash){
        this.consistentHash = consistentHash;
        this.id = id;

        fingerTable= new HashMap<>();
        for (int k = 1; k <=m ; k++){
            fingerTable.put(k,new Finger(consistentHash.getNode(k),"localhost",12345)); //todo change
        }
        this.m = m;
    }

    public int findSuccessor(int id) {
        if (id >= this.id && id <= successor)
            return successor;
        else {
            int closestPrecedingNode = closestPrecedingNode(id);
            return closestPrecedingNode.findSuccessor(id);
        }
    }

    public int closestPrecedingNode(int id) {
        for (int i = m; i >= 1; i--) {
            if (isInRange(fingerTable[i].getId(), this.id, id))
                return fingerTable[i].getId();
        }
        return id;
    }

    private boolean isInRange(int value, int start, int end) {
        // Implementation to check if value is within the range (start, end]
    }
}
