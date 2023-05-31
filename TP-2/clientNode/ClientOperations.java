import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.SocketChannel;
import java.util.HashMap;
import java.util.Map;

public class ClientOperations {
    private final SocketChannel socket;
    private Map<String,String> result ;
    private final String client;

    public ClientOperations(String client, InetSocketAddress address) throws IOException {
        this.result= new HashMap<>();
        this.socket = SocketChannel.open();
        this.client = client;
        socket.connect(address);
        if (socket.isConnected()){
            System.out.println("Socket is connected to Session Node!");
        }
    }
    public Map<String,String> returnResults(){
        //Format map to a string for printing
        return this.result;
    }
    private byte[] serializeWrites(String key,String value){
        return new byte[4];
    }

    private byte[] serializeNReads(String[] key){
        return new byte[4];
    }

    private Map<String,String> deserializeNReads(byte[] bytes){
        return new HashMap<>();
    }

    public void writeValue(String key, String value) throws IOException {
        ByteBuffer bf = ByteBuffer.allocate(200);
        byte[] content = serializeWrites(key,value);
        bf.put(content);
        bf.flip();
        socket.write(bf.duplicate());
        bf.clear();
        socket.read(bf);
        System.out.println("Written <k,v>: (" + key + "," +value + ")" );
        result.put(key,value);
    }
    public Map<String,String> readNValues(String[] keys) throws IOException {
        ByteBuffer bf = ByteBuffer.allocate(200);
        byte[] content = serializeNReads(keys);
        bf.put(content);
        bf.flip();
        socket.write(bf.duplicate());
        bf.clear();
        socket.read(bf);
        bf.flip();
        Map<String,String> reads = deserializeNReads(bf.array());
        this.result.putAll(reads);
        return reads;
    }






}
