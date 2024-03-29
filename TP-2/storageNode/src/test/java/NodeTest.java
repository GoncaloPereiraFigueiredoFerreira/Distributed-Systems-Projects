import chord.NodeController;
import chord.hashing.HashingAlgorithm;
import chord.storage.DataStorage;
import chord.storage.Dependencie;
import org.junit.jupiter.api.Test;
import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;
import java.util.Random;

import static org.junit.Assert.assertEquals;
import static org.junit.Assert.assertNull;


class NodeTest {
    @Test
    void startChord() {
        NodeController loadBalancer = new NodeController(5,5);
        loadBalancer.run();
    }

    @Test
    void clientTest() {
        try (ZContext context = new ZContext()) {
            System.out.println(sendDealer(context,"tcp://localhost:5555",null,"getKey|pao|0"));
            //System.out.println(sendDealer(context,"tcp://localhost:5555",1949372803,"insertKey|key3|key1|0|value"));
        }
    }

    @Test
    void addNode() {
        try (ZContext context = new ZContext()) {
            System.out.println(sendReq(context,"tcp://localhost:5550",null,"add_node"));
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

    private static String sendReq(ZContext context, String destiny, Integer nodeId, String message) {
        ZMQ.Socket socket = context.createSocket(SocketType.REQ);
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

    private static String sendDealer(ZContext context, String destiny, Integer nodeId, String message) {
        ZMQ.Socket socket = context.createSocket(SocketType.DEALER);
        socket.connect(destiny);
        socket.sendMore("");
        socket.send(nodeId + "|" + message, 0);
        socket.recv(0);
        byte[] reply = socket.recv(0);
        context.destroySocket(socket);
        return new String(reply, ZMQ.CHARSET);
    }
}