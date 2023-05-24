package org.example.chord;

import org.example.ConsistentHash;

import java.util.HashMap;
import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

public class Node {
    @Override
    public String toString() {
        return "Node{" +
                "nodeAddress="+ nodeAddress+
                ",\n nodeId=" + nodeId +
                ",\n successor =" + successor +
                ",\n predecessor=" + predecessor +
                "}\n";
    }

    private final String nodeAddress;
    private int nodeId;
    //private final HashMap<Integer,Finger> fingerTable;
    private ConsistentHash consistentHash;
    //private final int m;
    private Finger successor;
    private Finger predecessor;

    public int getNodeId() {
        return nodeId;
    }

    public String getNodeAddress() {
        return nodeAddress;
    }

    public Finger getSuccessor() {
        return successor;
    }

    public Finger getPredecessor() {
        return predecessor;
    }
    /*
    public Node(int id, int m, ConsistentHash consistentHash){
        this.consistentHash = consistentHash;
        this.nodeId = id;

        fingerTable= new HashMap<>();
        for (int k = 1; k <=m ; k++){
            fingerTable.put(k,new Finger(consistentHash.getNode(k),"localhost",12345)); //todo change
        }
        this.m = m;
    }*/

    public Node(int nodeId,String nodeAddress) {
        this.nodeId = nodeId;
        this.nodeAddress = nodeAddress;
        this.successor = null;
        this.predecessor = null;
    }

    public String processJoinRequest(int newNodeId,String newNodeAddress) {
        Finger oldPrecessor = predecessor;
        if(oldPrecessor==null){
            oldPrecessor = new Finger(nodeId,nodeAddress);
        }

        if (successor != null) {
            if(true){ //todo STOP CONDITION
                predecessor = new Finger(newNodeId,newNodeAddress);
                return "STOP " + nodeId + " " + oldPrecessor.getId() + " " + oldPrecessor.getAddress();
            }
            else {
                return "REDIRECT " + successor.getId() + " " + successor.getAddress();
            }
        }
        else {
            predecessor = new Finger(newNodeId,newNodeAddress);
            return "STOP " + nodeId + " " + oldPrecessor.getId() +" "+ oldPrecessor.getAddress();
        }
    }

    public void updateSucessors(int newSuccessorId,String newSuccessorString,
                                int newPredecessorId, String newPredecessorString) {
        successor = new Finger(newSuccessorId,newSuccessorString);
        predecessor = new Finger(newPredecessorId,newPredecessorString);
    }

    public void updateSucessor(int newSuccessorId,String newSuccessorString) {
        successor = new Finger(newSuccessorId,newSuccessorString);
    }








    /*
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
    }*/
}
