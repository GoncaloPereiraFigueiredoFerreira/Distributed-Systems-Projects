package org.example.chord;

import org.example.HashingAlgorithm;
import org.example.chord.storage.DataStorage;
import org.example.chord.storage.Dependencie;
import org.example.chord.storage.Version;
import org.junit.Assert;
import org.junit.jupiter.api.Test;

import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;

import static org.junit.jupiter.api.Assertions.*;

class NodeTest {
    @Test
    void startChord() throws NoSuchAlgorithmException {
        for (int i=0;i<10;i++){
            int basePort = 5555;
            Thread thread = new Thread(new NodeRunner("tcp://localhost:"+(basePort+i*4),"tcp://localhost:5555",5));
            thread.start();
        }

        NodeRunner test = new NodeRunner("tcp://localhost:5562","tcp://localhost:5555",1);
        test.run();
        // Print the successor of each node
        //System.out.println("Node 2 successor: " + node2.getSuccessor());
       // System.out.println("Node 3 successor: " + node3.getSuccessor());
    }

    @Test
    void dataStorageTest() throws NoSuchAlgorithmException {
        DataStorage dataStorage = new DataStorage(new HashingAlgorithm(1));
        List<Dependencie> dependencies = new ArrayList<>();
        dataStorage.insertKey(dependencies,"key","v1");
        dataStorage.insertKey(dependencies,"key","v2");
        dataStorage.insertKey(dependencies,"key","v3");

        assertEquals("v1", dataStorage.getKey("key",0).getValue());
        assertEquals("v2", dataStorage.getKey("key",1).getValue());
        assertEquals("v3",dataStorage.getKey("key",2).getValue());
        assertEquals("v3",dataStorage.getKey("key").getValue());
        assertNull(dataStorage.getKey("key", 5));
        assertNull(dataStorage.getKey("nonExistentKey", 1));
        assertNull(dataStorage.getKey("nonExistentKey"));
    }
}