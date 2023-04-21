package causalop;

import io.reactivex.rxjava3.annotations.NonNull;
import io.reactivex.rxjava3.core.ObservableOperator;
import io.reactivex.rxjava3.core.Observer;
import io.reactivex.rxjava3.observers.DisposableObserver;

import java.util.*;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;
import java.util.logging.Logger;
import java.util.stream.Collectors;


public class CausalOperatorO<T> implements ObservableOperator<T, CausalMessage<T>> {
    private final int n;
    private VersionVector vv;
    private List<Integer> lastDeliveredKeys;
    private int sum;
    private List<CausalMessage<T>> messageBuffer;
    private Logger logger;

    public CausalOperatorO(int n, Logger logger) {
        this.n = n;
        this.vv = new VersionVector(n);
        this.sum = 0;
        this.messageBuffer = new ArrayList<>();
        this.logger=logger;
        this.lastDeliveredKeys = new ArrayList<>();
    }


    private boolean isOutDated(CausalMessage<T> m){
        VersionVector clock = m.vv;
        return vv.getVersion(m.j) + 1 > clock.getVersion(m.j);
    }
    private boolean check(CausalMessage<T> m){
        VersionVector clock = m.vv;

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

        for (Map.Entry<Integer,Integer> entry:messageDependencies.entrySet()){
            if(!(this.vv.getVV().get(entry.getKey())>entry.getValue())){
                this.lastDeliveredKeys = this.lastDeliveredKeys.stream().filter(e-> e!=entry.getKey()).collect(Collectors.toList());
            }
        }
        this.lastDeliveredKeys.add(m.j);
    }


    public VersionVector cbCast(int id){
        VersionVector vector = vv.cbcast(id,this.lastDeliveredKeys);
        this.lastDeliveredKeys.clear();
        return vector;
    }


    @Override
    public @NonNull Observer<? super CausalMessage<T>> apply(@NonNull Observer<? super T> down) throws Throwable {

        return new DisposableObserver<CausalMessage<T>>() {
            @Override
            public void onNext(@NonNull CausalMessage<T> message) {
                if(check(message)) {
                    deliver(message);
                    for (Iterator<CausalMessage<T>> it = messageBuffer.listIterator(); it.hasNext() && message.sumClock() - sum <= 1 ; ) {
                        CausalMessage<T> cm = it.next();
                        if (isOutDated(cm)) {
                            it.remove();
                        } else if (check(cm)) {
                            deliver(cm);
                            it.remove();
                            it = messageBuffer.listIterator();
                        }
                    }
                }
                else{
                    message.vv.calculateVectorSum(vv);
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

            public void deliver(@NonNull CausalMessage<T> message){
                sum++;
                vv.increaseVersion(message.j);
                evaluateDependencies(message);
                down.onNext(message.payload);
            }
        };
    }
}
