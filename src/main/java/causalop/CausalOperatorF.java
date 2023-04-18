package causalop;

import io.reactivex.rxjava3.annotations.NonNull;
import io.reactivex.rxjava3.core.FlowableOperator;
import org.reactivestreams.Subscriber;
import org.reactivestreams.Subscription;

import java.util.*;
import java.util.stream.Collectors;


public class CausalOperatorF<T> implements FlowableOperator<T, CausalMessage<T>> {
    private final int n;
    private VersionVector2 vv;
    private List<Integer> lastDeliveredKeys;
    private Map<Integer,Integer> dependencies;
    private List<CausalMessage<T>> messageBuffer;

    public CausalOperatorF(int n) {
        this.n = n;
        this.vv = new VersionVector2(n);
        this.messageBuffer = new ArrayList<>();
        this.lastDeliveredKeys = new ArrayList<>();
        this.dependencies = new HashMap<>();
    }

    public CausalOperatorF(int n,VersionVector2 vv) {
        this.n = n;
        this.vv = vv.Clone();
        this.messageBuffer = new ArrayList<>();
        this.lastDeliveredKeys = new ArrayList<>();
        this.dependencies = new HashMap<>();
    }


    private boolean isOutDated(CausalMessage<T> m){
        VersionVector2 clock = m.vv;
        boolean flag = false;
        if (vv.getVersion(m.j) + 1 > clock.getVersion(m.j)){
            flag = true;
        }
        return flag;
    }
    private boolean check(CausalMessage<T> m){
        VersionVector2 clock = m.vv;

        boolean flag = true;
        if (vv.getVersion(m.j) + 1 != clock.getVersion(m.j)){
            flag = false;
        }
        else{
            for (Integer key: clock.getKeys()){
                if (key != m.j && clock.getVersion(key) >vv.getVersion(key)){
                    flag = false;
                }
            }
        }
        return flag;
    }

    private void evaluateDependencies(CausalMessage<T> m){
        Map<Integer,Integer> messageDependencies = m.vv.getVV();
        messageDependencies.remove(m.j);

        boolean sameDependencies = this.dependencies.entrySet().equals(messageDependencies.entrySet());
        if (!sameDependencies) {
            this.lastDeliveredKeys.clear();
            this.dependencies = messageDependencies;
        }
        this.lastDeliveredKeys.add(m.j);
    }


    public VersionVector2 cbCast(int id){
        VersionVector2 vector = vv.cbcast(id,this.lastDeliveredKeys);
        this.lastDeliveredKeys.clear();
        this.dependencies.clear();
        return vector;
    }

    @Override
    public @NonNull Subscriber<? super CausalMessage<T>> apply(@NonNull Subscriber<? super T> down) throws Throwable {
        return new Subscriber<CausalMessage<T>>() {
            private long credits = 0;
            private Subscription parent;

            @Override
            public void onSubscribe(Subscription subscription) {
                parent = subscription;
                down.onSubscribe(new Subscription() {
                    @Override
                    public void request(long l) {
                        parent.request(1);
                        credits += l;
                    }

                    @Override
                    public void cancel() {

                    }
                });
            }

            @Override
            public void onNext(@NonNull CausalMessage<T> m) {
                messageBuffer.add(m);
                for ( Iterator<CausalMessage<T>> it = messageBuffer.listIterator();it.hasNext();){
                    CausalMessage<T> cm = it.next();
                    if(isOutDated(cm)){
                        it.remove();
                    }
                    else if (check(cm)){
                        vv.increaseVersion(m.j);
                        evaluateDependencies(m);
                        down.onNext(cm.payload);
                        it.remove();
                        it = messageBuffer.listIterator();
                    }
                }
                parent.request(1);
            }



            @Override
            public void onError(@NonNull Throwable e) {
                down.onError(e); // FIXME
            }

            @Override
            public void onComplete() {
                if (messageBuffer.size()!=0){
                    down.onError(new IllegalArgumentException()); // DK if this is what we should do
                }
                else down.onComplete();
            }
        };
    }
}
