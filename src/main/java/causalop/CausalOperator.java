package causalop;

import io.reactivex.rxjava3.annotations.NonNull;
import io.reactivex.rxjava3.core.ObservableOperator;
import io.reactivex.rxjava3.core.Observer;
import io.reactivex.rxjava3.observers.DisposableObserver;

import java.util.*;


public class CausalOperator<T> implements ObservableOperator<T, CausalMessage<T>> {
    private final int n;
    private int vv[];
    private int sum;
    private List<CausalMessage<T>> messageBuffer;

    public CausalOperator(int n) {
        this.n = n;
        this.vv = new int[n];
        this.sum = 0;
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


    private void processCausalMessage(CausalMessage<T> m,@NonNull Observer<? super T> down){
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
    }

    public int[] getAndIncrementVV(int id){
        sum++;
        vv[id]++;
        return vv;
    }

    @Override
    public @NonNull Observer<? super CausalMessage<T>> apply(@NonNull Observer<? super T> down) throws Throwable {
        return new DisposableObserver<CausalMessage<T>>() {
            @Override
            public void onNext(@NonNull CausalMessage<T> message) {
                if(check(message)) {
                    sum++;
                    vv[message.j]++;
                    down.onNext(message.payload);
                    for (Iterator<CausalMessage<T>> it = messageBuffer.listIterator(); it.hasNext() && message.sumClock() - sum <= 1 ; ) {
                        CausalMessage<T> cm = it.next();
                        if (isOutDated(cm)) {
                            it.remove();
                        } else if (check(cm)) {
                            sum++;
                            vv[message.j]++;
                            down.onNext(cm.payload);
                            it.remove();
                            it = messageBuffer.listIterator();
                        }
                    }
                }
                else{
                    messageBuffer.add(message);
                    Collections.sort(messageBuffer);//TODO melhorar inserçao
                }
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
