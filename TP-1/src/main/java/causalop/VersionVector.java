package causalop;

import java.nio.ByteBuffer;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Set;
import java.util.stream.Collectors;

public class VersionVector implements Cloneable{
    private Map<Integer,Integer> vv;
    private int vectorSum;
    public VersionVector(){
        this.vv = new HashMap<>();
        vectorSum = 0;
    }

    public VersionVector(Map<Integer, Integer> vv) {
        this.vv = new HashMap<>();
        this.vv.putAll(vv);
        vectorSum = 0;
    }

    public VersionVector(int nNodos){
        this.vv = new HashMap<>();
        for(int i=0;i<nNodos;i++){
            vv.put(i,0);
        }
        vectorSum = 0;
    }

    public VersionVector(int[] array){
        this.vv = new HashMap<>();
        for(int i=0;i<array.length;i++){
            vv.put(i,array[i]);
        }
        vectorSum = 0;
    }

    public Set<Integer> getKeys(){
        return vv.keySet();
    }

    public void calculateVectorSum(VersionVector vv2){
        VersionVector temp = vv2.clone();
        temp.getVV().forEach(this.vv::putIfAbsent);
        this.vectorSum=temp.vv.values().stream().mapToInt(e->e).sum();
    }

    public void calculateVectorSum(){
        this.vectorSum= this.vv.values().stream().mapToInt(e->e).sum();
    }

    public int getVersion(int nodo){
        return vv.get(nodo);
    }

    public void increaseVersion(int nodo){
        int atual = vv.get(nodo);
        this.vv.put(nodo,atual+1);
    }

    public int getLastSumClock(){
        if(this.vectorSum==0){
            calculateVectorSum();
        }
        return this.vectorSum;
    }

    public VersionVector cbcast(int nodo, List<Integer> lastKeys){ //improvement 1 aplicado
        increaseVersion(nodo);
        Map<Integer,Integer> toSendVV = new HashMap<>();
        toSendVV.put(nodo,this.vv.get(nodo));
        for (Integer key:lastKeys){
            if(key!=null && key!=nodo){
                toSendVV.put(key,this.vv.get(key));
            }
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
        }
    }
    public Map<Integer,Integer> getVV(){
        return this.vv.entrySet().stream().collect(Collectors.toMap(Map.Entry::getKey, Map.Entry::getValue));
    }

    @Override
    public VersionVector clone() {
        return new VersionVector(this.vv);
    }
}
