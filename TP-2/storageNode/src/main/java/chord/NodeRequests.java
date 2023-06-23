package chord;

import org.zeromq.ZContext;

import java.util.List;
import java.util.Objects;

public class NodeRequests{
    private final SocketStorage cache;
    private final ZContext context;
    public NodeRequests(ZContext context){
        this.context=context;
        this.cache = new SocketStorage(context);
    }
    public Finger findPredecessor(Finger node) {
        Finger predecessor = null;
        String message = "get_predecessor|" + node.getId();

        String replyString = sendRep(node.getAddress(), String.valueOf(node.getId()), message);

        String[] values = replyString.split("\\|");
        if (values[0].equals("get_predecessor_response")) {
            if (!Objects.equals(values[1], "null")) {
                int predecessorId = Integer.parseInt(values[1]);
                String predecessorAddress = values[2];
                predecessor = new Finger(predecessorId, predecessorAddress);
            }
        } else System.out.println("Erro");

        return predecessor;
    }


    public String notifyRequest(Finger origin, Finger destiny) {
        String message = "notify|" + origin.getId() + "|" + origin.getAddress();
        return sendRep(destiny.getAddress(), String.valueOf(destiny.getId()), message);
    }

    public void join_complete_Request(String loadBalancer,int nodeID){
        try {
            sendDealerWAck(loadBalancer,"","join_completed|"+nodeID);
        } catch (InterruptedException e) {
            throw new RuntimeException(e);
        }
    }


    public Finger find_successor_request(Integer id,Finger dest) {
        Finger successor = null;
        String nextNodeAddress = dest.getAddress();
        Integer nextNodeId = dest.getId();

        while (nextNodeAddress != null) {
            String message = "find_successor|" + id;

            String replyString = sendRep(nextNodeAddress, String.valueOf(nextNodeId), message);

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

        return successor;
    }

    private boolean sendDealerWAck(String destiny, String nodeId, String message) throws InterruptedException {
        while (true) {
            String response = cache.sendAndReceive(destiny,nodeId,message);
            if (!response.equals("ACK")) {
                Thread.sleep(100);  // Wait for a short interval before resending
            } else {
                break;  // Received ACK, exit the loop
            }
        }
        return true;
    }

    private String sendRep(String destiny, String nodeId, String message) {
        return cache.sendAndReceive(destiny,nodeId,message);
    }

    public void cleanCache(List<String> addresses){
        this.cache.removeIfNotContains(addresses);
    }
}
