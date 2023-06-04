package chord;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import java.util.Objects;

public class NodeRequests implements NodeRequestsInterface{
    private static final long TIMEOUT_MS = 10000;

    public Finger findPredecessor(Finger node){
        Finger predecessor = null;
        try (ZContext context = new ZContext()) {
            String message = "get_predecessor|" + node.getId();

            String replyString = sendRep(context,node.getAddress(),node.getId(),message);

            String[] values = replyString.split("\\|");
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
    public String notifyRequest(Finger origin, Finger destiny) {
        try (ZContext context = new ZContext()) {
            String message = "notify|" + origin.getId() + "|" + origin.getAddress();
            return sendRep(context,destiny.getAddress(),destiny.getId(),message);
        }
    }

    public void join_complete_Request(String loadBalancer,int nodeID){
        try (ZContext context = new ZContext()) {
            sendDealerWAck(context,loadBalancer,"","join_completed|"+nodeID);
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
                String message = "find_successor|" + id;

                String replyString = sendRep(context,nextNodeAddress,nextNodeId,message);

                String[] values = replyString.split("\\|");

                if (values[0].equals("successor")) {
                    int successorId = Integer.parseInt(values[1]);
                    String stop = values[2];
                    String successorAddress = values[3];

                    if (Objects.equals(stop, "true")) {
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

    private static boolean sendDealerWAck(ZContext context, String destiny, String nodeId, String message) throws InterruptedException {
        String replyString;

        ZMQ.Socket socket = context.createSocket(SocketType.REQ);
        socket.connect(destiny);

        ZMQ.Poller poller = context.createPoller(1);
        poller.register(socket, ZMQ.Poller.POLLIN);

        while (true) {
            socket.send(nodeId + "|" + message, 0);
            byte[] reply = socket.recv(0);
            replyString = new String(reply, ZMQ.CHARSET);
            if (!replyString.equals("ACK")) {
                Thread.sleep(100);  // Wait for a short interval before resending
            } else {
                break;  // Received ACK, exit the loop
            }
        }
        context.destroySocket(socket);
        return true;
    }

    private static String sendRep(ZContext context, String destiny, Integer nodeId, String message) {
        ZMQ.Socket socket = context.createSocket(SocketType.REQ);
        socket.connect(destiny);
        socket.send(nodeId + "|" + message, 0);
        byte[] reply = socket.recv(0);
        context.destroySocket(socket);
        return new String(reply, ZMQ.CHARSET);
    }
}
