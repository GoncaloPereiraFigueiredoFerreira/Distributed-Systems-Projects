package org.example.chord;

import org.junit.jupiter.api.Test;

import java.security.NoSuchAlgorithmException;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.*;

class NodeTest {

    @Test
    void join() throws InterruptedException, NoSuchAlgorithmException {
        Thread thread = new Thread(new NodeRunner("tcp://localhost:5555","tcp://localhost:5555",1));
        thread.start();

        // Create Chord nodes
        Thread thread2 = new Thread(new NodeRunner("tcp://localhost:5559","tcp://localhost:5555",1));
        thread2.start();

        //Thread thread3 = new Thread(new NodeRunner("tcp://localhost:5565","tcp://localhost:5555",1));
        //thread3.start();

       // System.out.println("Finished sleep");
        NodeRunner test = new NodeRunner("tcp://localhost:5562","tcp://localhost:5555",1);
        test.run();

        //Node node3 = new Node(3, null,port+3);

        // Join the Chord network
        //node2.join("tcp://localhost:"+port+1);
        //node3.join("tcp://localhost:"+port+2);

        // Print the successor of each node
        //System.out.println("Node 2 successor: " + node2.getSuccessor());
       // System.out.println("Node 3 successor: " + node3.getSuccessor());
    }
}