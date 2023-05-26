package org.example.chord;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import java.util.Objects;
import java.util.Random;

public class NodeRequests implements NodeRequestsInterface{
    private static Random rand = new Random(System.nanoTime());
    private final String identity = String.format(
            "%04X-%04X", rand.nextInt(), rand.nextInt()
    );

    public Finger findPredecessor(Finger node){
        Finger predecessor = null;
        try (ZContext context = new ZContext()) {
            String message = "get_predecessor " + node.getId();

            String replyString = sendDealer(context,identity,node.getAddress(),node.getId(),message);

            String[] values = replyString.split("\\s+");
            if (values[0].equals("get_predecessor_response")) {
                if(!Objects.equals(values[1], "null")) {
                    int predecessorId = Integer.parseInt(values[1]);
                    String predecessorAddress = values[2];
                    predecessor = new Finger(predecessorId, predecessorAddress);
                }
            } else System.out.println("Erro");
        }
        return predecessor;
    }

    @Override
    public void notifyRequest(Finger origin, Finger destiny) {
        try (ZContext context = new ZContext()) {
            String message = "notify " + origin.getId() + " " + origin.getAddress();
            sendDealerWAck(context,identity,destiny.getAddress(),destiny.getId(),message);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }


    public void join_start_Request(String startingNode){
        try (ZContext context = new ZContext()) {
            sendDealerWAck(context, identity,startingNode,null,"add_node");
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }

    public void join_complete_Request(String startingNode){
        try (ZContext context = new ZContext()) {
            sendDealerWAck(context, identity,startingNode,null,"join_completed");
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }


    public Finger find_successor_request(Integer id,Finger dest){
        Finger successor = null;
        try (ZContext context = new ZContext()) {
            String nextNodeAddress = dest.getAddress();
            Integer nextNodeId = dest.getId();

            while (nextNodeAddress!=null) {
                String message = "find_successor " + id;

                String replyString = sendDealer(context,identity,nextNodeAddress,nextNodeId,message);

                String[] values = replyString.split("\\s+");

                if (values[0].equals("successor")) {
                    int successorId = Integer.parseInt(values[1]);
                    String successorAddress = values[2];

                    if (Objects.equals(successorAddress, nextNodeAddress) && (nextNodeId==null || successorId == nextNodeId)) {
                        successor = new Finger(successorId, successorAddress);
                        nextNodeAddress = null;
                    } else { //Missed
                        nextNodeId = successorId;
                        nextNodeAddress = successorAddress;
                    }
                } else {
                    System.out.println("erro");
                }
            }
        }
        return successor;
    }

    private void sendDealerWAck(ZContext context,String identity, String destiny,Integer nodeId, String message) throws InterruptedException {
        String replyString = "";

        ZMQ.Socket socket = context.createSocket(SocketType.DEALER);
        socket.setIdentity(identity.getBytes(ZMQ.CHARSET));
        socket.connect(destiny);

        while (true) {
            socket.send(nodeId+" "+message, 0);
            byte[] reply = socket.recv(0);
            replyString = new String(reply, ZMQ.CHARSET);
            if(!replyString.equals("ACK")){
                Thread.sleep(100);
            }
            else break;
        }
        context.destroySocket(socket);
    }

    private String sendDealer(ZContext context,String identity, String destiny,Integer nodeId, String message){
        ZMQ.Socket socket = context.createSocket(SocketType.DEALER);
        socket.setIdentity(identity.getBytes(ZMQ.CHARSET));
        socket.connect(destiny);
        socket.send(nodeId+" "+message, 0);

        byte[] reply = socket.recv(0);

        context.destroySocket(socket);
        return  new String(reply, ZMQ.CHARSET);
    }
}
