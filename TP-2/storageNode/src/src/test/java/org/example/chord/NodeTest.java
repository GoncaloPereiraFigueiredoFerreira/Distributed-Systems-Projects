package org.example.chord;

import org.example.HashingAlgorithm;
import org.example.LoadBalancer;
import org.example.chord.storage.DataStorage;
import org.example.chord.storage.Dependencie;
import org.example.chord.storage.Version;
import org.junit.Assert;
import org.junit.jupiter.api.Test;
import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;
import java.util.Objects;
import java.util.Random;

import static org.junit.jupiter.api.Assertions.*;

class NodeTest {
    @Test
    void startChord() {
        LoadBalancer loadBalancer = new LoadBalancer(0,1);
        loadBalancer.run();
    }

    @Test
    void clientTest() {
        Random rand = new Random(System.nanoTime());
        String identity = String.format(
                "%04X-%04X", rand.nextInt(), rand.nextInt()
        );
        //insertKey
        try (ZContext context = new ZContext()) {
            //System.out.println(sendDealer(context, identity,"tcp://localhost:5555",null,"insertKey|key|key1|0|key2|1|value"));
            System.out.println(sendDealer(context, identity,"tcp://localhost:5559",1731348717,"getKey|key|3"));
        }
    }

    @Test
    void addNode() {
        Random rand = new Random(System.nanoTime());
        String identity = String.format(
                "%04X-%04X", rand.nextInt(), rand.nextInt()
        );
        //insertKey
        try (ZContext context = new ZContext()) {
            //System.out.println(sendDealer(context, identity,"tcp://localhost:5555",400210732,"insertKey|key|key1|0|key2|1|value"));
            System.out.println(sendDealer(context, identity,"tcp://localhost:5550",null,"add_node"));
        }
    }

    @Test
    void dataStorageTest() throws NoSuchAlgorithmException {
        DataStorage dataStorage = new DataStorage(new HashingAlgorithm(1),0);
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

    private static String sendDealer(ZContext context, String identity, String destiny, Integer nodeId, String message) {
        ZMQ.Socket socket = context.createSocket(SocketType.DEALER);
        socket.setIdentity(identity.getBytes(ZMQ.CHARSET));
        socket.connect(destiny);
        socket.send(nodeId + "|" + message, 0);

        ZMQ.Poller poller = context.createPoller(1);
        poller.register(socket, ZMQ.Poller.POLLIN);

        if (poller.poll(1000) <= 0) {
            // Timeout occurred, no reply received
            context.destroySocket(socket);
            return "Error";
        }
        if (poller.pollin(0)) {
            byte[] reply = socket.recv(0);
            context.destroySocket(socket);
            return new String(reply, ZMQ.CHARSET);
        }
        context.destroySocket(socket);
        return "";
    }
}