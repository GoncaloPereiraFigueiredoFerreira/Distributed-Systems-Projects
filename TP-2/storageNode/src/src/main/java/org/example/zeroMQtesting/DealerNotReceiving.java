package org.example.zeroMQtesting;

import org.example.chord.Node;
import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;
import org.zeromq.ZMsg;

import java.util.Random;

public class DealerNotReceiving {
    public static void main(String[] args) {
        // Start the ZeroMQ REP socket to receive join requests
        try (ZContext context = new ZContext()) {
            ZMQ.Socket frontend = context.createSocket(SocketType.ROUTER);
            frontend.bind("tcp://localhost:5555");

            Random rand = new Random(System.nanoTime());
            String identity = String.format(
                    "%04X-%04X", rand.nextInt(), rand.nextInt()
            );
            ZMQ.Socket socket = context.createSocket(SocketType.DEALER);
            socket.setIdentity(identity.getBytes(ZMQ.CHARSET));
            socket.connect("tcp://localhost:5555");
            socket.send("hello", 0);

            ZMsg msg = ZMsg.recvMsg(frontend);
            msg.destroy();
        }

    }


}

