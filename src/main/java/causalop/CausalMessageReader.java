package causalop;

import io.reactivex.rxjava3.annotations.NonNull;
import io.reactivex.rxjava3.core.ObservableOperator;
import io.reactivex.rxjava3.core.Observer;
import io.reactivex.rxjava3.observers.DisposableObserver;

import java.nio.ByteBuffer;
import java.nio.charset.StandardCharsets;
import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;


public class CausalMessageReader implements ObservableOperator<Message, ByteBuffer>{

    @Override
    public @NonNull Observer<? super ByteBuffer> apply(@NonNull Observer<? super Message> observer) throws Throwable {
        return new DisposableObserver<>() {
            @Override
            public void onNext(@NonNull ByteBuffer b) {
                try {
                    Message m = CausalMessage.fromByteBuffer(b.duplicate());
                    observer.onNext(m);
                }catch (Exception e){
                    var s = StandardCharsets.UTF_8.decode(b.duplicate());
                    observer.onNext(new ClientMessage(s));
                }
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
