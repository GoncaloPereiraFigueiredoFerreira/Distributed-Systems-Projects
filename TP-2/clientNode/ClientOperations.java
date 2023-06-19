import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.SocketChannel;
import java.util.HashMap;
import java.util.Map;

public class ClientOperations {
    private final SocketChannel socket;
    private Map<String,String> result ;

    public ClientOperations(InetSocketAddress address) throws IOException {
        this.result= new HashMap<>();
        this.socket = SocketChannel.open();
        socket.connect(address);
        if (socket.isConnected()){
            System.out.println("Socket is connected to Session Node!");
        }
    }
    public Map<String,String> returnResults(){
        //Format map to a string for printing
        return this.result;
    }

    public void login(String username) throws IOException {
        ByteBuffer bf = ByteBuffer.allocate(200);
        byte[] content = ClientProtocol.serializeLogin(username);
        bf.put(content);
        bf.flip();
        socket.write(bf.duplicate());
        bf.clear();
        socket.read(bf);
        bf.flip();
        if (ClientProtocol.deserializeGeneralResponse(bf.array()))
            System.out.println("Login response received!");;
    }


    public void logout() throws IOException {
        ByteBuffer bf = ByteBuffer.allocate(200);
        byte[] content = ClientProtocol.serializeLogout();
        bf.put(content);
        bf.flip();
        socket.write(bf.duplicate());
        bf.clear();
        socket.read(bf);
        if (ClientProtocol.deserializeGeneralResponse(bf.array()))
            System.out.println("Logout response received!");;
    }

    public Boolean writeValue(String key, String value) throws IOException {
        ByteBuffer bf = ByteBuffer.allocate(200);
        byte[] content = ClientProtocol.serializeWrites(key,value);
        bf.put(content);
        bf.flip();
        socket.write(bf.duplicate());
        bf.clear();
        socket.read(bf);
        Boolean ret = ClientProtocol.deserializeGeneralResponse(bf.array());
        if (ret) {
            System.out.println("Written <k,v>: (" + key + "," + value + ")");
            result.put(key, value);
        }
        return ret;
    }
    public Map<String,String> readNValues(String[] keys) throws IOException {
        ByteBuffer bf = ByteBuffer.allocate(200);
        byte[] content = ClientProtocol.serializeNReads(keys);
        bf.put(content);
        bf.flip();
        socket.write(bf.duplicate());
        bf.clear();
        socket.read(bf);
        bf.flip();
        Map<String,String> reads = ClientProtocol.deserializeNReads(bf);
        if (reads!=null){
            this.result.putAll(reads);
            return reads;
        }
        return null;
        //
    }

    public Boolean addDataServer() throws IOException {
        ByteBuffer bf = ByteBuffer.allocate(200);
        byte[] content = ClientProtocol.serializeAddNode();
        bf.put(content);
        bf.flip();
        socket.write(bf.duplicate());
        bf.clear();
        socket.read(bf);
        bf.flip();
        Boolean added = ClientProtocol.deserializeGeneralResponse(bf.array());
        if (added) {
            System.out.println("Server was added");
        }
        else System.out.println("Server was not added");
        return added;
    }






}
