package chord;

import org.zeromq.SocketType;
import org.zeromq.ZContext;
import org.zeromq.ZMQ;

import java.util.concurrent.locks.ReentrantLock;

public class LockedSocket {
    private ZMQ.Socket socket;
    private final ReentrantLock lock;

    public LockedSocket(ZContext context, String destiny) {
        socket = context.createSocket(SocketType.REQ);
        socket.connect(destiny);
        lock = new ReentrantLock();
    }

    public String sendAndReceive(String nodeId, String message) {
        lock.lock();
        try {
            socket.send(nodeId + "|" + message, 0);
            byte[] reply = socket.recv(0);
            return new String(reply, ZMQ.CHARSET);

        } finally {
            lock.unlock();
        }
    }

    public void destroy(ZContext context) {
        lock.lock();
        try {
            context.destroySocket(socket);
            this.socket=null;
        } finally {
            lock.unlock();
        }
    }
}
