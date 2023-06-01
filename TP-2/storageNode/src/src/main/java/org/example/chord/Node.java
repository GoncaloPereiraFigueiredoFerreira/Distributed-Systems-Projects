package org.example.chord;

import org.example.ConsistentHash;
import org.example.HashingAlgorithm;
import org.example.chord.storage.DataStorage;
import org.example.chord.storage.Version;

import javax.xml.crypto.Data;
import java.security.NoSuchAlgorithmException;
import java.util.*;

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
    private final DataStorage dataStorage;
    private Finger predecessor;

    public Boolean isMaster() {
        return master;
    }

    public Node(Boolean master, int nodeId, String nodeAddress) throws NoSuchAlgorithmException {
        this.master = master;
        this.working=master; // not mistaken, if not master then it is not working on initialization
        this.nodeId = nodeId;
        this.nodeAddress = nodeAddress;
        this.predecessor = null;
        this.dataStorage= new DataStorage(new HashingAlgorithm(1));
        this.m = 31;
        this.fingerTable = new Finger[m+1];
        for (int i = 1; i<=m; i++)
            fingerTable[i]=myFinger();
    }

    public Finger getPredecessor() {
        return predecessor;
    }

    public Boolean isWorking() {
        return working;
    }


    public void joinRing(String startingNode){
        nodeRequests.join_start_Request(startingNode);
        this.fingerTable[1] = nodeRequests.find_successor_request(nodeId,new Finger(null,startingNode));
        working = true;
        nodeRequests.join_complete_Request(startingNode);
    }

    public Map<String,Boolean> fix_Fingers(Integer next){
        Map<String,Boolean> booleans = new HashMap<>();
        boolean reset = false;
        if(next>m){
            reset = true;
            next=1;
        }

        int result = (int) ((nodeId + Math.pow(2, next - 1)) % Math.pow(2, 31));
        Finger fingerResult = nodeRequests.find_successor_request(result,myFinger());
        boolean stable = fingerResult.equals(fingerTable[next]);
        fingerTable[next] = fingerResult;

        booleans.put("reset",reset);
        booleans.put("stable",stable);
        return booleans;
    }

    public void insertKey(String key, Version version){
        this.dataStorage.insertKey(key,version);
    }

    public int getLastKeyVersion(String key) {
        return this.dataStorage.getLastKeyVersion(key);
    }
    public Version getKey(String value,int version) {
        return this.dataStorage.getKey(value,version);
    }
    public Boolean stabilize() {
        boolean stable = true;
        Finger x;
        if(Objects.equals(fingerTable[1], myFinger())) {
            x = predecessor; //evitar requests quando o pedido é local
        }
        else {
            x = nodeRequests.findPredecessor(fingerTable[1]);
        }
        if (x != null && (nodeId==fingerTable[1].getId() || isInRange(x.getId(), nodeId, fingerTable[1].getId()))) {
            fingerTable[1] = x; // Update current node's successor to be its predecessor
            stable=false;
        }
        if(!Objects.equals(fingerTable[1], myFinger())) {
            dataStorage.insertFromString(nodeRequests.notifyRequest(myFinger(), fingerTable[1])); //Addition suss
        }
        return stable;
    }

    public Map<String,List<Version>> notify(Finger n){
        if(predecessor==null || isInRange(n.getId(), predecessor.getId(), nodeId))
            predecessor=n;
        return dataStorage.moveKeys(n.getId()); //TODO suss?
    }

    public FingerSuccessorPair findSuccessor(int id) {
        if (isInRange(id, nodeId, fingerTable[1].getId()))
            return new FingerSuccessorPair(fingerTable[1], true);
        return closestPrecedingNode(id);
    }

    public boolean isRightSuccessor(int id) { //TODO suss?
        if(predecessor==null)
            return true; //TODO Might be wrong
        return isInRange(id, predecessor.getId(), nodeId);
    }

    private FingerSuccessorPair closestPrecedingNode(int id) {
        if(m>1) {
            for (int i = m; i >= 1; i--) {
                if (isInRange(fingerTable[i].getId(), nodeId,id)){
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
