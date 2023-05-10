package causalop;

import java.nio.ByteBuffer;

public class CausalMessage<T> implements Message, Comparable<CausalMessage<T>> {
    int j;
    int[] vv;
    public T payload;

    public CausalMessage(T payload, int j, int... v) {
        this.payload = payload;
        this.j = j;
        this.vv = v;
    }

    public ByteBuffer toByteBuffer() {
        int payloadLength = 0;
        byte[] payloadBytes = null;
        if (payload != null) {
            payloadBytes = payload.toString().getBytes();
            payloadLength = payloadBytes.length;
        }
        ByteBuffer buffer = ByteBuffer.allocate(1 + Integer.BYTES + Integer.BYTES + Integer.BYTES + Integer.BYTES * vv.length + payloadLength);
        buffer.put((byte) 0);
        buffer.putInt(j);
        buffer.putInt(vv.length);
        for (int v : vv) {
            buffer.putInt(v);
        }
        if (payload != null) {
            buffer.put(payloadBytes);
        }
        buffer.flip();
        return buffer;
    }

    public static <T> CausalMessage<T> fromByteBuffer(ByteBuffer buffer) throws Exception {
        byte start = buffer.get();
        if(start!=(byte) 0){
            throw new Exception();
        }
        int j = buffer.getInt();
        int vvLength = buffer.getInt();
        int[] vv = new int[vvLength];
        for (int i = 0; i < vvLength; i++) {
            vv[i] = buffer.getInt();
        }

        byte[] payloadBytes = new byte[buffer.remaining()];
        buffer.get(payloadBytes);
        String payloadString = new String(payloadBytes);
        T payload = null;
        if (!payloadString.isEmpty()) {
            payload = (T) payloadString;
        }
        return new CausalMessage<T>(payload, j, vv);
    }

    @Override
    public int getType() {
        return 0;
    }

    public int sumClock(){
        int sum = 0;
        for (int i:vv){
            sum = sum + i;
        }
        return sum;
    }


    @Override
    public int compareTo(CausalMessage<T> o) {
        return this.sumClock() - o.sumClock();
    }
}
