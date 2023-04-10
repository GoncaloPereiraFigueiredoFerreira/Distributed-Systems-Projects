package causalop;

import io.reactivex.rxjava3.annotations.NonNull;
import io.reactivex.rxjava3.core.FlowableOperator;
import io.reactivex.rxjava3.core.ObservableOperator;
import io.reactivex.rxjava3.core.Observer;
import io.reactivex.rxjava3.observers.DisposableObserver;
import org.reactivestreams.Subscriber;
import org.reactivestreams.Subscription;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.concurrent.Flow;


public class CausalOperatorF<T> implements FlowableOperator<T, CausalMessage<T>> {
    private final int n;
    private int vv[];
    private List<CausalMessage<T>> messageBuffer;

    public CausalOperatorF(int n) {
        this.n = n;
        this.vv = new int[n];
        for (int i =0; i<n;i++) this.vv[i]=0; // Just to be sure it starts the array with 0s
        this.messageBuffer = new ArrayList<>();
    }


    private boolean isOutDated(CausalMessage<T> m){
        int[] clock = m.vv;
        boolean flag = false;
        if (vv[m.j] + 1 > clock[m.j]){
            flag = true;
        }
        return flag;
    }
    private boolean check(CausalMessage<T> m){
        int[] clock = m.vv;

        boolean flag = true;
        if (vv[m.j] + 1 != clock[m.j]){
            flag = false;
        }
        else{
            for(int i=0; i<n; i++) {
                if (i != m.j && clock[i] >vv[i]){
                    flag = false;
                }
            }
        }
        return flag;
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
                        vv[m.j]++;
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
