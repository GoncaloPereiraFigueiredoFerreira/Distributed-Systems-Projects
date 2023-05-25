package org.example.chord;

import org.example.DataStorage;
import org.zeromq.*;

import java.net.Socket;
import java.security.NoSuchAlgorithmException;
import java.util.*;
import java.util.stream.Collectors;

public class NodeRunner implements Runnable {
    private final String startingNodeAddress;
    private Boolean isAddingNode;
    private final Integer defaultNode;
    private static Random rand = new Random(System.nanoTime());
    private DataStorage dataStorage;
    private final Map<Integer,Node> nodes;
    private final String nodeAddress;
    public NodeRunner(int id,String address,String startingNodeAddress,Integer numberOfReplicas) throws NoSuchAlgorithmException {
        this.startingNodeAddress = startingNodeAddress;
        this.isAddingNode = false;
        this.dataStorage = new DataStorage(1,numberOfReplicas);
        this.nodeAddress = address;

        List<Integer> ids = dataStorage.addNode(address);
        defaultNode = ids.get(0);
        nodes = new HashMap<>();
        for (Integer nodeId:ids){
            nodes.put(nodeId,new Node(nodeId,address));
        }
    }

    private Boolean isMaster(){
        return startingNodeAddress==null;
    }

    @Override
    public void run() {
        // Start the ZeroMQ REP socket to receive join requests
        try (ZContext context = new ZContext()) {
            ZMQ.Socket frontend = context.createSocket(SocketType.ROUTER);
            frontend.bind(nodeAddress);


            for (Node node:nodes.values()) {
                if ( !(isMaster() && node.getNodeId() == defaultNode)) {
                    Thread newThread = new Thread(() -> {
                        connectToRing(startingNodeAddress, node);
                    });
                    newThread.start();
                }
            }

            while (!Thread.currentThread().isInterrupted()) {
                processRequest(frontend);
            }
        }
    }

    public void connectToRing(String startingNode,Node node){
        String nextNodeAddress = startingNode;
        if(isMaster())
            nextNodeAddress= nodeAddress;
        Integer nextNodeId = null;
        try (ZContext context = new ZContext()) {
            while (nextNodeAddress!=null) {

                ZMQ.Socket socket = context.createSocket(SocketType.DEALER);
                String identity = String.format(
                        "%04X-%04X", rand.nextInt(), rand.nextInt()
                );
                socket.setIdentity(identity.getBytes(ZMQ.CHARSET));
                socket.connect(nextNodeAddress);


                String message = "JOIN " + nextNodeId + " " + node.getNodeId() + " " + node.getNodeAddress();
                socket.send(message, 0);

                byte[] reply = socket.recv(0);
                String replyString = new String(reply, ZMQ.CHARSET);


                String[] values = replyString.split("\\s+");
                switch (values[0]) {
                    case "STOP" -> {
                        int sucessorId = Integer.parseInt(values[1]);
                        String sucessorAddress = nextNodeAddress;
                        int predecessorId = Integer.parseInt(values[2]);
                        String predecessorAddress = values[3];

                        node.updateSucessors(sucessorId, sucessorAddress, predecessorId, predecessorAddress);

                        message = "REPLACESUCESSOR " + predecessorId + " " + node.getNodeId() + " " + node.getNodeAddress();
                        if (Objects.equals(predecessorAddress, nextNodeAddress)) {
                            socket.send(message, 0);
                            socket.recv();
                        } else {
                            context.destroySocket(socket);
                            sendMsgWithDealer(context, identity,predecessorAddress,message);
                        }
                        context.destroySocket(socket);
                        if(isMaster())
                            sendMsgWithDealer(context, identity,nodeAddress,"JOINCOMPLETED");
                        else sendMsgWithDealer(context, identity,startingNodeAddress,"JOINCOMPLETED");
                        nextNodeAddress = null;
                    }
                    case "NACK" -> {
                        Thread.sleep(500);
                    }
                    case "REDIRECT" -> {
                        nextNodeId = Integer.parseInt(values[1]);
                        nextNodeAddress = values[2];
                    }
                    default -> System.out.println("erro");
                }
                context.destroySocket(socket);
            }
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
        System.out.println("Node "+ node.getNodeId() +" finished connection to ring {"+nodes.values()+"}");
    }

    private void sendMsgWithDealer(ZContext context,String identity, String destiny,String message) {
        ZMQ.Socket socket = context.createSocket(SocketType.DEALER);
        socket.setIdentity(identity.getBytes(ZMQ.CHARSET));

        socket.connect(destiny);
        socket.send(message, 0);

        socket.recv();
        context.destroySocket(socket);
    }

    public void processRequest(ZMQ.Socket socket) {
        System.out.println("Listening {"+nodes.values()+"}");
        ZMsg msg = ZMsg.recvMsg(socket);
        ZFrame address = msg.pop();
        ZFrame content = msg.pop();
        assert (content != null);
        msg.destroy();

        String returnMessage = processMessageContent(new String(content.getData(), ZMQ.CHARSET));
        ZFrame newContent;
        newContent = new ZFrame(Objects.requireNonNullElse(returnMessage, "ack"));

        address.send(socket,ZFrame.REUSE + ZFrame.MORE);
        newContent.send(socket, ZFrame.REUSE);
        newContent.destroy();

        address.destroy();
        content.destroy();
    }

    private String processMessageContent(String requestString) {
        String[] values = requestString.split("\\s+");

        switch (values[0]) {
            case "JOIN" -> {
                System.out.println("Node address: "+ this.nodeAddress  + ":received join");

                if(isMaster()){
                    if(isAddingNode) {
                        return "NACK";
                    }
                    else isAddingNode=true;
                }
                Integer wantedNode;
                if(Objects.equals(values[1], "null"))
                    wantedNode=defaultNode;
                else wantedNode = Integer.parseInt(values[1]);

                int originNodeId = Integer.parseInt(values[2]);
                String nodeAddress = values[3];

                return nodes.get(wantedNode).processJoinRequest(originNodeId, nodeAddress);
            }
            case "JOINCOMPLETED" -> {
                System.out.println("Node address: "+ this.nodeAddress  + ": received JoinComplete");
                isAddingNode=false;
                return "ACK";
            }
            case "REPLACESUCESSOR" -> {
                System.out.println("Node address: "+ this.nodeAddress  + ":received replace");
                Integer wantedNode;
                if(Objects.equals(values[1], "null"))
                    wantedNode=defaultNode;
                else wantedNode = Integer.parseInt(values[1]);

                int newSuccessorId = Integer.parseInt(values[2]);
                String nodeAddress = values[3];
                nodes.get(wantedNode).updateSucessor(newSuccessorId,nodeAddress);
                return null;
            }
            default -> {
                System.out.println("Node address: "+ this.nodeAddress  + ":received invalid request {" + values[0] + "}");
                return null;
            }
        }
    }
}
