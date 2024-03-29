package chord;

import chord.hashing.HashingAlgorithm;
import chord.storage.DataStorage;
import chord.storage.Version;
import org.zeromq.ZContext;

import java.security.NoSuchAlgorithmException;
import java.util.*;
import java.util.concurrent.locks.ReentrantReadWriteLock;
import java.util.stream.Collectors;

public class Node {
    @Override
    public String toString() {
        return "Node{" +
                "nodeAddress="+ nodeAddress+
                ",\n nodeId=" + nodeId +
                ",\n fingers =" + Arrays.toString(fingerTable) +
                "}\n";
    }
    private final NodeRequests nodeRequests;
    private Boolean working;
    private final String nodeAddress;
    private final int nodeId;
    private final Finger[] fingerTable;
    private final int m;
    private final DataStorage dataStorage;

    public Node(int nodeId, String nodeAddress, Boolean isFirst, ZContext context) throws NoSuchAlgorithmException {
        this.working=isFirst; // not mistaken, if not master then it is not working on initialization
        this.nodeId = nodeId;
        this.nodeAddress = nodeAddress;
        this.nodeRequests = new NodeRequests(context);
        this.dataStorage= new DataStorage(new HashingAlgorithm(1),nodeId);
        this.m = 31;
        this.fingerTable = new Finger[m+1];
        fingerTable[0]=null;
        for (int i = 1; i<=m; i++)
            fingerTable[i]=myFinger();
    }

    public Finger getPredecessor() {
        return fingerTable[0];
    }

    public Boolean isWorking() {
        return working;
    }

    public int getNodeId() {
        return nodeId;
    }

    public List<String> getAllAddresses(){
        List<String> addresses = new ArrayList<>();
        for (Finger finger : fingerTable) {
            if(finger!=null){
                addresses.add(finger.getAddress());
            }
        }
        addresses.add(this.nodeAddress);
        return addresses.stream().distinct().collect(Collectors.toList());
    }

    public String getNodeAddress() {
        return nodeAddress;
    }

    public void joinRing(Integer idDest, String addressDest, String loadBalancerAddress){
        this.fingerTable[1] = nodeRequests.find_successor_request(nodeId,new Finger(idDest,addressDest));
        working = true;
        nodeRequests.join_complete_Request(loadBalancerAddress,nodeId);
    }

    public Map<String,Boolean> fix_Fingers(Integer next){
        Map<String,Boolean> booleans = new HashMap<>();
        boolean reset = false;
        if(next>m){
            reset = true;
            next=1;
        }

        if(next==1){
            nodeRequests.cleanCache(this.getAllAddresses());
        }
        int result = (int) ((nodeId + Math.pow(2, next - 1)) % Math.pow(2, 31));
        Finger fingerResult = nodeRequests.find_successor_request(result,myFinger());
        boolean stable = fingerResult.equals(fingerTable[next]);
        fingerTable[next] = fingerResult;

        booleans.put("reset",reset);
        booleans.put("stable",stable);
        return booleans;
    }

    public int insertKey(String key, Version version){
        return this.dataStorage.insertKey(key,version);
    }

    public int getLastKeyVersion(String key) {
        return this.dataStorage.getLastKeyVersion(key);
    }
    public Version getKey(String value, int version) {
        return this.dataStorage.getKey(value,version);
    }
    public Boolean stabilize() {
        boolean stable = true;
        Finger x;
        if(Objects.equals(fingerTable[1], myFinger())) {
            x = fingerTable[0]; //evitar requests quando o pedido é local
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
        Finger predecessor = fingerTable[0];
        if(predecessor==null || isInRange(n.getId(), predecessor.getId(), nodeId))
            fingerTable[0]=n;
        return dataStorage.moveKeys(n.getId());
    }

    public FingerSuccessorPair findSuccessor(int id) {
        if (isInRange(id, nodeId, fingerTable[1].getId()))
            return new FingerSuccessorPair(fingerTable[1], true);
        return closestPrecedingNode(id);
    }

    public boolean isRightSuccessor(int id) {
        Finger predecessor = fingerTable[0];
        if(predecessor==null)
            return true;
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
