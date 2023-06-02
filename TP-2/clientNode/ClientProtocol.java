import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.HashMap;
import java.util.Map;

public class ClientProtocol {
    public static byte[] serializeLogin(String username) {
        ByteBuffer bf = ByteBuffer.allocate(200);
        bf.put("li|".getBytes(StandardCharsets.UTF_8));
        bf.put(username.getBytes(StandardCharsets.UTF_8));
        bf.putChar('\n');
        bf.flip();
        return bf.array();
    }
    public static byte[] serializeLogout() {
        ByteBuffer bf = ByteBuffer.allocate(4);
        bf.put("lo\n".getBytes(StandardCharsets.UTF_8));
        bf.flip();
        return bf.array();
    }
    public static byte[] serializeWrites(String key,String value){
        ByteBuffer bf = ByteBuffer.allocate(200);
        bf.put("w|".getBytes(StandardCharsets.UTF_8));
        bf.put(key.getBytes(StandardCharsets.UTF_8));
        bf.put("|".getBytes(StandardCharsets.UTF_8));
        bf.put(value.getBytes(StandardCharsets.UTF_8));
        bf.putChar('\n');
        bf.flip();
        return bf.array();
    }

    public static byte[] serializeNReads(String[] keys){
        ByteBuffer bf = ByteBuffer.allocate(200);
        bf.put("r".getBytes(StandardCharsets.UTF_8));
        for (int i =0; i<keys.length;i++){
            bf.put("|".getBytes(StandardCharsets.UTF_8));
            bf.put(keys[i].getBytes(StandardCharsets.UTF_8));
        }
        bf.putChar('\n');
        return bf.array();
    }
    public static boolean deserializeGeneralResponse(byte[] response){
        ByteBuffer bf = ByteBuffer.wrap(response);
        char o = bf.getChar();
        char k = bf.getChar();
        return o == 'o' && k == 'k';
    }
    public static Map<String,String> deserializeNReads(byte[] response){
        ByteBuffer bf = ByteBuffer.wrap(response);
        Map<String,String> ret = new HashMap<>();
        char o = bf.getChar();
        char k = bf.getChar();
        if  (o == 'o' && k == 'k'){
            bf.getChar(); //remove pipe
            while (bf.hasRemaining()){
                StringBuilder key = new StringBuilder();
                StringBuilder value = new StringBuilder();
                char c;
                while(bf.hasRemaining() && ((c = bf.getChar()) !='|')){
                    key.append(c);
                }
                while(bf.hasRemaining() && ((c = bf.getChar()) !='|')){
                    value.append(c);
                }
                ret.put(key.toString(), value.toString());
            }
            return ret;
        }
        else return null;
    }



}
