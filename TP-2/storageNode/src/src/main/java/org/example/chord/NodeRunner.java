package org.example.chord;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

public class NodeRunner implements Runnable {
    private final Node node;
    private final int port;

    public NodeRunner(int id,String sucessor,int basePort){
        this.port = basePort+id;
        this.node = new Node(id,sucessor,port);
    }

    @Override
    public void run() {
        // Start the ZeroMQ REP socket to receive join requests
        try (ZContext context = new ZContext()) {
            ZMQ.Socket socket = context.createSocket(SocketType.REP);
            socket.bind("tcp://localhost:"+port);

            while (!Thread.currentThread().isInterrupted()) {
                // Wait for a join request
                byte[] request = socket.recv(0);
                String requestString = new String(request, ZMQ.CHARSET);


                // Process the join request
                if (requestString.startsWith("JOIN")) {
                    int nodeId = Integer.parseInt(requestString.substring(5));
                    String successorAddress = node.processJoinRequest(nodeId);

                    // Reply with the successor node's address
                    if(successorAddress!=null){
                        socket.send(successorAddress.getBytes(ZMQ.CHARSET), 0);
                    }
                    else {
                        socket.send("".getBytes(ZMQ.CHARSET), 0);
                    }
                } else if (requestString.startsWith("NEWSUCESSOR")) {
                    int nodeId = Integer.parseInt(requestString.substring(12));
                    node.processNewSucessor(nodeId);
                } else { // Invalid request
                    socket.send("Invalid request".getBytes(ZMQ.CHARSET), 0);
                }
            }
        }
    }
}
