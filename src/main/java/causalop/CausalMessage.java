package causalop;

import java.nio.ByteBuffer;

public class CausalMessage<T> implements Message, Comparable<CausalMessage<T>> {
    int j;
    VersionVector vv;
    public T payload;

    public CausalMessage(T payload, int j, VersionVector v) {
        this.payload = payload;
        this.j = j;
        this.vv = v.clone();
    }

    public ByteBuffer toByteBuffer() {
        int payloadLength = 0;
        byte[] payloadBytes = null;
        if (payload != null) {
            payloadBytes = payload.toString().getBytes();
            payloadLength = payloadBytes.length;
        }
        ByteBuffer buffer = ByteBuffer.allocate(1 + Integer.BYTES + vv.toByteBuffer().limit() + payloadLength);
        buffer.put((byte) 0);

        buffer.putInt(j);

        buffer.put(vv.toByteBuffer());

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

        VersionVector vv = new VersionVector();
        vv.fromByteBuffer(buffer);

        byte[] payloadBytes = new byte[buffer.remaining()];
        buffer.get(payloadBytes);
        String payloadString = new String(payloadBytes);
        T payload = null;
        if (!payloadString.isEmpty()) {
            payload = (T) payloadString;
        }
        return new CausalMessage<T>(payload, j, vv);
    }

    public int getType() {
        return 0;
    }

    public int sumClock(){
        return vv.getLastSumClock();
    }

    @Override
    public int compareTo(CausalMessage<T> o) {
        return this.sumClock() - o.sumClock();
    }
}
