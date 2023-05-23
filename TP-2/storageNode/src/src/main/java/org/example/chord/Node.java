package org.example.chord;

import org.example.ConsistentHash;

import java.util.HashMap;
import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

public class Node {
    private final Integer CHORD_NODE_PORT;
    private int nodeId;
    //private final HashMap<Integer,Finger> fingerTable;
    private ConsistentHash consistentHash;
    //private final int m;
    String successor;

    public String getSuccessor() {
        return successor;
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

    public Node(int nodeId, String successor,int port) {
        this.nodeId = nodeId;
        this.CHORD_NODE_PORT= port;
        this.successor = successor;
    }

    public void join(String knownNodeAddress) {
        try (ZContext context = new ZContext()) {
            ZMQ.Socket socket = context.createSocket(SocketType.REQ);
            socket.connect(knownNodeAddress);

            // Send a join request to the known node
            socket.send(String.format("JOIN %d", nodeId).getBytes(ZMQ.CHARSET), 0);

            // Wait for the reply containing the successor node's address
            byte[] reply = socket.recv(0);
            String successorAddress = new String(reply, ZMQ.CHARSET);
            if(!successorAddress.equals("")){
                successor = successorAddress;
            }
            else successor = null;
        }
    }

    public String processJoinRequest(int newNodeId) {
        if (successor != null) {
            // Create a ZeroMQ REQ socket to communicate with the successor node
            try (ZContext context = new ZContext()) {
                ZMQ.Socket socket = context.createSocket(SocketType.REQ);
                socket.connect(successor);

                // Send a join request to the successor node
                socket.send(String.format("JOIN %d", newNodeId).getBytes(ZMQ.CHARSET), 0);

                // Wait for the reply containing the new successor node's address
                byte[] reply = socket.recv(0);
                return new String(reply, ZMQ.CHARSET);
            }
        } else return null;
    }

    public void processNewSucessor(int newNodeId) {
        // The current node is the only node in the network
        successor = String.format("tcp://localhost:%d", CHORD_NODE_PORT+newNodeId);
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
