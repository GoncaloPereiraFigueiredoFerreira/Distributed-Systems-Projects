package org.example;

import org.example.chord.NodeRunner;
import org.zeromq.*;

import java.security.NoSuchAlgorithmException;
import java.util.*;

public class LoadBalancer implements Runnable{
    HashSet<Integer> allIds = new HashSet<>();
    HashSet<Integer> addingNodes = new HashSet<>();
    int basePort;
    int counter = 0;
    String loadBalancerAddress;

    private HashingAlgorithm hashingAlgorithm;
    @Override
    public void run() {
        basePort = 5555;
        loadBalancerAddress = "tcp://localhost:5550";
        try {
            hashingAlgorithm= new HashingAlgorithm(1);
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException(e);
        }

        startChordNode(true);
        for (int i = 0; i < 9; i++) {
            startChordNode(false);
        }

        try (ZContext context = new ZContext()) {
            ZMQ.Socket frontend = context.createSocket(SocketType.ROUTER);
            frontend.bind(loadBalancerAddress);

            while (!Thread.currentThread().isInterrupted()) {
                processRequest(frontend);
            }
        }

    }

    public void processRequest(ZMQ.Socket socket) {
        ZMsg msg = ZMsg.recvMsg(socket);
        ZFrame address = msg.pop();
        ZFrame content = msg.pop();
        assert (content != null);
        msg.destroy();

        String[] values = new String(content.getData(), ZMQ.CHARSET).split("\\|");
        if(Objects.equals(values[0], ""))
            values = Arrays.copyOfRange(values, 1, values.length);
        String returnMessage = processMessageContent(values);
        ZFrame newContent;
        newContent = new ZFrame(Objects.requireNonNullElse(returnMessage, "empty"));

        address.send(socket,ZFrame.REUSE + ZFrame.MORE);
        newContent.send(socket, ZFrame.REUSE);
        newContent.destroy();

        address.destroy();
        content.destroy();
    }

    private String processMessageContent(String[] values)   {
        switch (values[0]) {
            case "join_completed" -> {
                System.out.println("JoinComplete from: "+ values[1]);
                int nodeId = Integer.parseInt(values[1]);
                addingNodes.remove(nodeId);
                return "ACK";
            }
            case "add_node" -> {
                System.out.println("New node request");
                if(addingNodes.size()>0){
                    return "NACK";
                } else {
                    startChordNode(false);
                    return "ACK";
                }
            }
            default -> {
                System.out.println("Received invalid request");
                return null;
            }
        }
    }
    public void startChordNode(Boolean firstExecution)  {
        String address = "tcp://localhost:" + basePort;
        List<Integer> ids = generateReplicaIds(address,5);
        Thread thread;
        try {
            thread = new Thread(new NodeRunner(firstExecution,address, "tcp://localhost:5555",ids,loadBalancerAddress));
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException(e);
        }
        thread.start();
        addingNodes.addAll(ids);

        basePort+=4;
    }

    private List<Integer> generateReplicaIds(String address,int numberOfReplicas){
        List<Integer> ids = new ArrayList<>();
        for (int i = 0; i <numberOfReplicas; i++) {
            Integer key = hashingAlgorithm.hash(address + i);
            while (allIds.contains(key)){
                key = hashingAlgorithm.hash(address + i + counter++);
            }
            ids.add(key);
            allIds.add(key);
        }
        return ids;
    }
}
