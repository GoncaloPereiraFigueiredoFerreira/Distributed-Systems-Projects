package org.example.chord;

import org.example.ConsistentHash;
import org.example.HashingAlgorithm;
import org.example.chord.storage.DataStorage;
import org.example.chord.storage.Version;
import org.zeromq.*;

import java.security.NoSuchAlgorithmException;
import java.util.*;

public class NodeRunner implements Runnable {
    private final String startingNodeAddress;
    private Boolean isAddingNode;
    private final Integer defaultNode;
    private final Map<Integer,Node> nodes;
    private final HashingAlgorithm hashingAlgorithm;
    private final String nodeAddress;
    public NodeRunner(String address,String startingNodeAddress,Integer numberOfReplicas) throws NoSuchAlgorithmException {
        this.startingNodeAddress = startingNodeAddress;
        this.isAddingNode = false;
        this.nodeAddress = address;

        this.hashingAlgorithm = new HashingAlgorithm(1);
        List<Integer> ids = generateReplicaIds(numberOfReplicas);

        defaultNode = ids.get(0);
        nodes = new HashMap<>();
        for (Integer nodeId:ids){
            Boolean isMaster = Objects.equals(nodeId, defaultNode) && Objects.equals(startingNodeAddress, nodeAddress);
                nodes.put(nodeId,new Node(isMaster,nodeId,address));
        }
    }

    private List<Integer> generateReplicaIds(Integer numberOfReplicas){
        List<Integer> ids = new ArrayList<>();
        for (int i = 0; i <numberOfReplicas; i++) {
            Integer key = hashingAlgorithm.hash(nodeAddress + i);
            ids.add(key);
        }
        return ids;
    }
    @Override
    public void run() {
        // Start the ZeroMQ REP socket to receive join requests
        try (ZContext context = new ZContext()) {
            ZMQ.Socket frontend = context.createSocket(SocketType.ROUTER);
            frontend.bind(nodeAddress);

            Thread newThread = new Thread(() -> {
                for (Node node : nodes.values()) {
                    if (!node.isMaster()) {
                        node.joinRing(startingNodeAddress);
                    }
                }
            });
            newThread.start();

            Thread periodicCounter = new Thread(() -> {
                try {
                    periodicStabilization(100);
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
            });
            periodicCounter.start();

            Thread periodicFixCounter = new Thread(() -> {
                try {
                    periodicFingerFix(100);
                } catch (InterruptedException e) {
                    throw new RuntimeException(e);
                }
            });
            periodicFixCounter.start();

            while (!Thread.currentThread().isInterrupted()) {
                processRequest(frontend);
            }
        }
    }

    public void periodicStabilization(long duration) throws InterruptedException {
        long variableDuration = duration;
        while (!Thread.currentThread().isInterrupted()){
            Thread.sleep(variableDuration);
            boolean allStable = true;
            for (Node n:nodes.values())
                if (n.isWorking())
                    if(!n.stabilize())
                        allStable=false;
            if(!allStable)
                variableDuration=duration;
            else {
                System.out.println("periodicStabilization stable");
                variableDuration = duration*5;
            }
        }
    }

    public void periodicFingerFix(long duration) throws InterruptedException {
        int next = 1;
        long variableDuration = duration;
        while (!Thread.currentThread().isInterrupted()){
            Thread.sleep(variableDuration);

            boolean reset = false;
            boolean allStable = true;
            for (Node n:nodes.values())
                if (n.isWorking()) {
                    Map<String, Boolean> booleans = n.fix_Fingers(next);
                    reset = booleans.get("reset");
                    if(!booleans.get("stable"))
                        allStable=false;
                }

            if (reset)
                next=2;
            else next++;

            if(!allStable)
                variableDuration=duration;
            else {
                System.out.println("fixFinger stable");
                variableDuration = duration*5;
            }
        }
    }


    public void processRequest(ZMQ.Socket socket) {
        ZMsg msg = ZMsg.recvMsg(socket);
        ZFrame address = msg.pop();
        ZFrame content = msg.pop();
        assert (content != null);
        msg.destroy();

        String returnMessage = processMessage(new String(content.getData(), ZMQ.CHARSET));
        ZFrame newContent;
        newContent = new ZFrame(Objects.requireNonNullElse(returnMessage, "empty"));

        address.send(socket,ZFrame.REUSE + ZFrame.MORE);
        newContent.send(socket, ZFrame.REUSE);
        newContent.destroy();

        address.destroy();
        content.destroy();
    }

    private String processMessage(String requestString) {
        String[] values = requestString.split("\\|");
        Integer wantedNode;
        if(Objects.equals(values[0], "null"))
            wantedNode=defaultNode;
        else wantedNode = Integer.parseInt(values[0]);


        String[] processedValues = new String[values.length - 1];
        System.arraycopy(values, 1, processedValues, 0, processedValues.length);

        return processMessageContent(processedValues,nodes.get(wantedNode));
    }

    private String processMessageContent(String[] values,Node workingNode) {
        switch (values[0]) {
            case "find_successor" -> {
                System.out.println("Node address: "+ this.nodeAddress  + ":received find_successor");
                int originNodeId = Integer.parseInt(values[1]);

                FingerSuccessorPair successor = workingNode.findSuccessor(originNodeId);
                return "successor|" + successor.getFinger().getId() + "|" + successor.isFound() + "|"+ successor.getFinger().getAddress();
            }
            case "notify" -> {
                System.out.println("Node address: "+ this.nodeAddress  + ":received notify");
                int originNodeId = Integer.parseInt(values[1]);
                String originNodeAddress = values[2];

                Map<String,List<Version>> keysToSend = workingNode.notify(new Finger(originNodeId,originNodeAddress));
                return DataStorage.keysToString(keysToSend);
            }
            case "add_node" -> {
                System.out.println("Node address: "+ this.nodeAddress  + ":received add_node");
                if(workingNode.isMaster() && !isAddingNode) {
                    isAddingNode = true;
                    return "ACK";
                }
                else return "NACK";
            }
            case "join_completed" -> {
                System.out.println("Node address: "+ this.nodeAddress  + ": received JoinComplete");
                if(workingNode.isMaster() && isAddingNode) {
                    isAddingNode=false;
                    return "ACK";
                }
                else return "NACK";
            }
            case "get_predecessor" -> {
                Finger predecessor = workingNode.getPredecessor();
                if(predecessor==null){
                    return "get_predecessor_response|null";
                }
                else return "get_predecessor_response|" + predecessor.getId() + "|" + predecessor.getAddress();
            }
            case "insertKey" -> {
                String key = values[1];
                int hashValue = hashingAlgorithm.hash(key);
                if(workingNode.isRightSuccessor(hashValue)) {
                    workingNode.insertKey(values[1], Version.fromStrings(Arrays.copyOfRange(values, 2, values.length)));
                    return "ACK";
                }
                else{
                    FingerSuccessorPair successor = workingNode.findSuccessor(hashValue);
                    return "successor|" + successor.getFinger().getId() + "|"+ successor.getFinger().getAddress();
                }
            }
            case "getLastKeyVersion" -> {
                String key = values[1];
                int hashValue = hashingAlgorithm.hash(key);
                if(workingNode.isRightSuccessor(hashValue)) {
                    return String.valueOf(workingNode.getLastKeyVersion(key));
                }
                else{
                    FingerSuccessorPair successor = workingNode.findSuccessor(hashValue);
                    return "successor|" + successor.getFinger().getId() + "|"+ successor.getFinger().getAddress();
                }
            }
            case "getKey" -> {
                String key = values[1];
                int hashValue = hashingAlgorithm.hash(key);
                if(workingNode.isRightSuccessor(hashValue)) {
                    int version = Integer.parseInt(values[2]);
                    return String.valueOf(workingNode.getKey(key,version));
                }
                else{
                    FingerSuccessorPair successor = workingNode.findSuccessor(hashValue);
                    return "successor|" + successor.getFinger().getId() + "|"+ successor.getFinger().getAddress();
                }
            }

            default -> {
                System.out.println("Node address: "+ this.nodeAddress  + ":received invalid request {" + values[0] + "}");
                return null;
            }
        }
    }
}
