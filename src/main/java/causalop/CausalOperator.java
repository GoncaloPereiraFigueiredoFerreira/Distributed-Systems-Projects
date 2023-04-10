package causalop;

import io.reactivex.rxjava3.annotations.NonNull;
import io.reactivex.rxjava3.core.ObservableOperator;
import io.reactivex.rxjava3.core.Observer;
import io.reactivex.rxjava3.observers.DisposableObserver;

import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;


public class CausalOperator<T> implements ObservableOperator<T, CausalMessage<T>> {
    private final int n;
    private int vv[];
    private List<CausalMessage<T>> messageBuffer;

    public CausalOperator(int n) {
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

    @Override
    public @NonNull Observer<? super Message> apply(@NonNull Observer<? super T> down) throws Throwable {
        return new DisposableObserver<Message>() {
            @Override
            public void onNext(@NonNull Message message) {
                switch (message.getType()){
                    case 0:
                        CausalMessage<T> cm1 = (CausalMessage<T>) message;
                        processCausalMessage(cm1,down);
                        break;
                    case 1:
                        ClientMessage<T> cm2 = (ClientMessage<T>) message;
                        down.onNext(cm2.message);
                        break;
                    default:
                        down.onError(new Exception());
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
