package causalop;

import causalop.CausalMessage;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.channels.SocketChannel;
import java.util.ArrayList;
import java.util.List;

public class CBCast<T> {
    int[] vv;
    private List<SocketChannel> channels;
    private int identifier;
    public CBCast(int nChannels, int id) throws IOException {
        this.vv = new int[nChannels];
        this.identifier = id;
        this.channels = new ArrayList<>();
        for (int i=0 ; i<nChannels ; i++){
            if(i!=id){
                SocketChannel channel = SocketChannel.open();
                channel.connect(new InetSocketAddress("localhost", 8888+i));
                channels.add(channel);
            }
        }
    }

    public void broadCast(T payload) throws IOException {
        vv[identifier]++;
        CausalMessage<T> message = new CausalMessage<>(payload,identifier,vv);
        for(SocketChannel channel:channels){
            channel.write(message.toByteBuffer());
        }
    }
}
