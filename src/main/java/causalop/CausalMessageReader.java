package causalop;

import io.reactivex.rxjava3.annotations.NonNull;
import io.reactivex.rxjava3.core.ObservableOperator;
import io.reactivex.rxjava3.core.Observer;
import io.reactivex.rxjava3.observers.DisposableObserver;

import java.nio.ByteBuffer;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;


public class CausalMessageReader<T> implements ObservableOperator<CausalMessage<T>, ByteBuffer>{

    @Override
    public @NonNull Observer<? super ByteBuffer> apply(@NonNull Observer<? super CausalMessage<T>> observer) throws Throwable {
        return new DisposableObserver<>() {
            @Override
            public void onNext(@NonNull ByteBuffer b) {
                observer.onNext(CausalMessage.fromByteBuffer(b));
            }



            @Override
            public void onError(@NonNull Throwable e) {
                observer.onError(e); // FIXME
            }

            @Override
            public void onComplete() {
                observer.onComplete(); // FIXME
            }
        };
    }
}
