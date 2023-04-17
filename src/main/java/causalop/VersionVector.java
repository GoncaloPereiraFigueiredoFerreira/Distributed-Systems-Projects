package causalop;

import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.stream.Collectors;

public class VersionVector {
    private Map<Integer,Integer> vv;
    private Map<Integer,Boolean> updates;

    public VersionVector(){
        this.vv = new HashMap<>();
        this.updates = new HashMap<>();
    }
    public VersionVector(Map<Integer, Integer> vv,Map<Integer, Boolean> vu) {
        this.vv = new HashMap<>();
        this.updates = new HashMap<>();
        // using iterator
        this.vv.putAll(vv);
        this.updates.putAll(vu);
    }

    public VersionVector(Map<Integer, Integer> vv) {
        this.vv = new HashMap<>();
        this.vv.putAll(vv);
    }

    public VersionVector(int nNodos){
        this.vv = new HashMap<>();
        this.updates = new HashMap<>();
        for(int i=0;i<nNodos;i++){
            vv.put(i,0);
            updates.put(i,false);
        }
    }

    public VersionVector(int[] array){
        this.vv = new HashMap<>();
        this.updates = new HashMap<>();
        for(int i=0;i<array.length;i++){
            vv.put(i,array[i]);
            updates.put(i,false);
        }
    }

    public int getVersion(int nodo){
        return vv.get(nodo);
    }

    public void increaseVersion(int nodo){
        int atual = vv.get(nodo);
        this.vv.put(nodo,atual+1);
        this.updates.put(nodo,false);
    }

    public int sumClock(){
        return vv.values().stream().mapToInt(e->e).sum();
    }

    public VersionVector cbcast(int nodo){ //improvement 1 aplicado
        increaseVersion(nodo);
        List<Integer> toSendKeys = updates.entrySet().stream().filter(e-> !e.getValue()).map(Map.Entry::getKey).toList();
        Map<Integer,Integer> toSendVV = new HashMap<>();
        for (Integer chave: toSendKeys){
            toSendVV.put(chave,this.vv.get(chave));
            this.updates.put(chave,true);
        }
        return new VersionVector(toSendVV);
    }

    public ByteBuffer toByteBuffer() {
        ByteBuffer buffer = ByteBuffer.allocate(4 + vv.size() * 8); // 8 bytes per entry (int key, int value)
        buffer.putInt(vv.size());
        for (Map.Entry<Integer, Integer> entry : vv.entrySet()) {
            buffer.putInt(entry.getKey());
            buffer.putInt(entry.getValue());
        }
        buffer.flip(); // switch to read mode
        return buffer;
    }



    public void fromByteBuffer(ByteBuffer buffer) {
        int size = buffer.getInt();
        for (int i=0;i<size;i++){
            int key = buffer.getInt();
            int value = buffer.getInt();
            vv.put(key, value);
            updates.put(key,false);
        }
    }
    public Map<Integer,Integer> getVV(){
        return this.vv;
    }
    public VersionVector Clone(){
        return new VersionVector(this.vv,this.updates);
    }

}
