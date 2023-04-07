package causalop;

public class CausalMessage<T> {
    int j;
    int[] vv;
    public T payload;

    public CausalMessage(T payload, int j, int... v) {
        this.payload = payload;
        this.j = j;
        this.vv = v;
    }

}
