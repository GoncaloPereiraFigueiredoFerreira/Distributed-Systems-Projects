package chord;

public interface NodeRequestsInterface {
    void join_complete_Request(String loadBalancer,int nodeID);
    Finger find_successor_request(Integer id,Finger dest);
    Finger findPredecessor(Finger node);

    String notifyRequest(Finger origin,Finger destiny);
}
