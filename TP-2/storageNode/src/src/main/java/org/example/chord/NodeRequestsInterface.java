package org.example.chord;

public interface NodeRequestsInterface {
    void join_start_Request(String startingNode);
    void join_complete_Request(String startingNode);
    Finger find_successor_request(Integer id,Finger dest);
    Finger findPredecessor(Finger node);

    String notifyRequest(Finger origin,Finger destiny);
}
