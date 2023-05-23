package org.example.chord;

import org.junit.jupiter.api.Test;

import static org.junit.jupiter.api.Assertions.*;

class NodeTest {

    @Test
    void join() {
        int port = 5555;

        Thread thread = new Thread(new NodeRunner(0,null,port));
        thread.start();

        // Create Chord nodes
        Node node1 = new Node(1, null,port+1);
        //Node node2 = new Node(2, null,port+2);
        //Node node3 = new Node(3, null,port+3);

        // Join the Chord network
        node1.join("tcp://localhost:"+(port+0));
        //node2.join("tcp://localhost:"+port+1);
        //node3.join("tcp://localhost:"+port+2);

        // Print the successor of each node
        System.out.println("Node 1 successor: " + node1.getSuccessor());
        //System.out.println("Node 2 successor: " + node2.getSuccessor());
       // System.out.println("Node 3 successor: " + node3.getSuccessor());
    }
}