package org.example.chord;

import org.junit.jupiter.api.Test;

import java.security.NoSuchAlgorithmException;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.*;

class NodeTest {

    @Test
    void join() throws InterruptedException, NoSuchAlgorithmException {
        for (int i=0;i<10;i++){
            int basePort = 5555;
            Thread thread = new Thread(new NodeRunner("tcp://localhost:"+(basePort+i*4),"tcp://localhost:5555",5));
            thread.start();
        }
        //Thread thread2 = new Thread(new NodeRunner("tcp://localhost:5570","tcp://localhost:5555",1));
        //thread2.start();

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