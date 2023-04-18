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



public class CausalOperatorO<T> implements ObservableOperator<T, CausalMessage<T>> {
    private final int n;
    private VersionVector2 vv;
    private List<Integer> lastDeliveredKeys;
    private Map<Integer,Integer> dependencies;
    private int sum;
    private List<CausalMessage<T>> messageBuffer;
    private Logger logger;

    public CausalOperatorO(int n, Logger logger) {
        this.n = n;
        this.vv = new VersionVector2(n);
        this.sum = 0;
        this.messageBuffer = new ArrayList<>();
        this.logger=logger;
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
            for(int i=0; i<n; i++) {
                if (i != m.j && clock.getVersion(i) >vv.getVersion(i)){
                    flag = false;
                }
            }
        }
        return flag;
    }

    public VersionVector2 cbCast(int id){
        return vv.cbcast(id,this.lastDeliveredKeys);
    }

    private void evaluateDependencies(CausalMessage<T> m){
        Map<Integer,Integer> messageDependencies = m.vv.getVV();
        if (messageDependencies.size()>1){
            messageDependencies.remove(m.j);
        }
        boolean sameDependencies = true;
        for(Map.Entry<Integer,Integer> entry :this.dependencies.entrySet()){
            if(!Objects.equals(messageDependencies.get(entry.getKey()), entry.getValue())){
                sameDependencies = false;
                break;
            }
        }
        if (!sameDependencies) {
            this.lastDeliveredKeys.clear();
            this.dependencies = messageDependencies;
        }
        this.lastDeliveredKeys.add(m.j);
    }
    @Override
    public @NonNull Observer<? super CausalMessage<T>> apply(@NonNull Observer<? super T> down) throws Throwable {
        return new DisposableObserver<CausalMessage<T>>() {
            @Override
            public void onNext(@NonNull CausalMessage<T> message) {
                if(check(message)) {
                    sum++;
                    vv.increaseVersion(message.j);
                    down.onNext(message.payload);
                    for (Iterator<CausalMessage<T>> it = messageBuffer.listIterator(); it.hasNext() && message.sumClock() - sum <= 1 ; ) {
                        CausalMessage<T> cm = it.next();
                        if (isOutDated(cm)) {
                            it.remove();
                        } else if (check(cm)) {
                            sum++;
                            evaluateDependencies(cm);
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
