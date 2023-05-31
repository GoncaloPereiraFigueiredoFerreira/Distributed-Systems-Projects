package org.example.chord;

import java.util.Arrays;
import java.util.Objects;
import java.util.AbstractMap;

public class Node {
    @Override
    public String toString() {
        return "Node{" +
                "nodeAddress="+ nodeAddress+
                ",\n nodeId=" + nodeId +
                ",\n successors =" + Arrays.toString(fingerTable) +
                ",\n predecessor=" + predecessor +
                "}\n";
    }
    private final NodeRequestsInterface nodeRequests = new NodeRequests();
    private final Boolean master;
    private Boolean working;
    private final String nodeAddress;
    private int nodeId;
    private final Finger[] fingerTable;
    private final int m;
    private Finger predecessor;

    public int getNodeId() {
        return nodeId;
    }

    public String getNodeAddress() {
        return nodeAddress;
    }

    public Finger getSuccessor(){
        return fingerTable[0];
    }

    public Boolean isMaster() {
        return master;
    }

    public Node(Boolean master, int nodeId, String nodeAddress) {
        this.master = master;
        this.working=master; // not mistaken, if not master then it is not working on initialization
        this.nodeId = nodeId;
        this.nodeAddress = nodeAddress;
        this.predecessor = null;
        this.m = 31;
        this.fingerTable = new Finger[m+1];
        for (int i = 0; i<=m; i++)
            fingerTable[i]=myFinger();
    }

    public Finger getPredecessor() {
        return predecessor;
    }

    public Boolean isWorking() {
        return working;
    }

    public void updateSuccessor(int newSuccessorId, String newSuccessorAddress) {
        fingerTable[0] = new Finger(newSuccessorId,newSuccessorAddress);
    }

    public void joinRing(String startingNode){
        nodeRequests.join_start_Request(startingNode);
        this.fingerTable[0] = nodeRequests.find_successor_request(nodeId,new Finger(null,startingNode));
        working = true;
        nodeRequests.join_complete_Request(startingNode);
    }

    public boolean fix_Fingers(Integer next){
        boolean reset = false;
        if(next>m){
            reset = true;
            next=1;
        }

        int result = (int) ((nodeId + Math.pow(2, next - 1)) % Math.pow(2, 31));
        Finger fingerResult = nodeRequests.find_successor_request(result,myFinger());
        fingerTable[next] = fingerResult;
        return reset;
    }

    public void stabilize() {
        Finger x;
        if(Objects.equals(fingerTable[0], myFinger())) {
            x = predecessor; //evitar requests quando o pedido é local
        }
        else {
            x = nodeRequests.findPredecessor(fingerTable[0]);
        }
        if (x != null && (nodeId==fingerTable[0].getId() || isInRange(x.getId(), nodeId, fingerTable[0].getId()))) {
            fingerTable[0] = x; // Update current node's successor to be its predecessor
        }
        if(!Objects.equals(fingerTable[0], myFinger())) {
            nodeRequests.notifyRequest(myFinger(), fingerTable[0]);
        }
    }

    public void notify(Finger n){
        if(predecessor==null || isInRange(n.getId(), predecessor.getId(), nodeId))
            predecessor=n;
    }

    public FingerSuccessorPair findSuccessor(int id) {
        if (isInRange(id, nodeId, fingerTable[0].getId()))
            return new FingerSuccessorPair(fingerTable[0], true);
        return closestPrecedingNode(id);
    }

    private FingerSuccessorPair closestPrecedingNode(int id) {
        if(m>1) {
            for (int i = m; i >= 0; i--) {
                if (isInRange(fingerTable[i].getId(), id,nodeId)){
                    if(!fingerTable[i].equals(myFinger()))
                        return new FingerSuccessorPair(fingerTable[i], false);
                }
            }
        }
        return new FingerSuccessorPair(myFinger(), true);
    }

    private Finger myFinger(){
        return new Finger(nodeId,nodeAddress);
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
