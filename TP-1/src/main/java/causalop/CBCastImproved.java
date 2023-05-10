package causalop;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.channels.SocketChannel;
import java.util.ArrayList;
import java.util.List;

public class CBCastImproved<T> {
    int[] vv;
    private List<Association> channels;
    private int identifier;

    private class Association{
        public int identifier;
        public SocketChannel channel;

        public Association(int i, SocketChannel channel) {
            this.identifier= i;
            this.channel = channel;
        }
    }

    public CBCastImproved(int nChannels, int id) throws IOException {
        this.vv = new int[nChannels];
        this.identifier = id;
        this.channels = new ArrayList<>();
        for (int i=0 ; i<nChannels ; i++){
            if(i!=id){
                SocketChannel channel = SocketChannel.open();
                channel.connect(new InetSocketAddress("localhost", 8888+i));
                channels.add(new Association(i,channel));
            }
        }
    }

    public void broadCast(T paylaod) throws IOException {
        vv[identifier]++;
        CausalMessage<T> message = new CausalMessage<>(paylaod,identifier,vv);
        for(Association association:channels){
            association.channel.write(message.toByteBuffer());
        }
    } //TODO acabar esta class toda basicamente...
}
