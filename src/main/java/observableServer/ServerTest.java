package observableServer;

import causalop.CausalMessage;
import causalop.VersionVector;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.SocketChannel;

public class ServerTest {
    public static void main(String args[]) throws IOException {
        try{
            SocketChannel channel = SocketChannel.open();
            channel.connect(new InetSocketAddress("localhost", 12340));

            // Send a message to the server
            VersionVector vv = new VersionVector(2);
            vv.increaseVersion(1);
            CausalMessage<String> message = new CausalMessage<>("a", 1, vv);
            ByteBuffer buffer = message.toByteBuffer();
            channel.write(buffer);
            buffer.clear();

            channel.close();
        }
        catch (IOException e){
            e.printStackTrace();
        }
    }
}
