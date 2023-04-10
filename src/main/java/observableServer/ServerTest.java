package observableServer;

import causalop.CausalMessage;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.SocketChannel;

public class ServerTest {
    public static void main(String args[]) throws IOException {
        try{
            SocketChannel channel = SocketChannel.open();
            channel.connect(new InetSocketAddress("localhost", 12345));

            // Send a message to the server
            CausalMessage<String> message = new CausalMessage<>("a", 1, 0, 1,0,0,0);
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
