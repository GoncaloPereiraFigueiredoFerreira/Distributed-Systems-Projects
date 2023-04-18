package causalop;

import io.reactivex.rxjava3.core.Observable;
import observableServer.ServerStarter;
import org.junit.Assert;
import org.junit.Test;

import java.util.logging.Logger;

public class CausalOpTestO {
    VersionVector2 vv1; // 0,1
    VersionVector2 vv2; // 1,0
    VersionVector2 vv3; // 1,2
    Logger logs = Logger.getLogger(ServerStarter.class.getName());

    public void setup(){
        vv1 = new VersionVector2(new int[]{0, 1});
        vv2 = new VersionVector2(new int[]{1, 0});
        vv3 = new VersionVector2(new int[]{1, 2});
    }


    @Test
    public void testOk() {
        setup();
        var l = Observable.just(
                        new CausalMessage<>("a", 1, vv1),
                        new CausalMessage<>("b", 0, vv2),
                        new CausalMessage<>("c", 1, vv3)
                )
                .lift(new CausalOperatorO<>(2,logs))
                .toList().blockingGet();

        Assert.assertArrayEquals(l.toArray(), new String[]{"a","b","c"});
    }

    @Test
    public void testReorder() {
        setup();
        var l = Observable.just(
                        new CausalMessage<String>("c", 1, vv3),
                        new CausalMessage<String>("a", 1, vv1),
                        new CausalMessage<String>("b", 0, vv2)
                )
                .lift(new CausalOperatorO<String>(2,logs))
                .toList().blockingGet();

        Assert.assertArrayEquals(l.toArray(), new String[]{"a","b","c"});
    }

    @Test
    public void testDupl() {
        setup();
        var l = Observable.just(
                        new CausalMessage<String>("a", 1, vv1),
                        new CausalMessage<String>("b", 0, vv2),
                        new CausalMessage<String>("a", 1, vv1),
                        new CausalMessage<String>("c", 1, vv3)
                )
                .lift(new CausalOperatorO<String>(2,logs))
                .toList().blockingGet();

        Assert.assertArrayEquals(l.toArray(), new String[]{"a","b","c"});
    }

    @Test(expected = IllegalArgumentException.class)
    public void testGap() {
        setup();
        var l = Observable.just(
                        new CausalMessage<String>("c", 1, vv3),
                        new CausalMessage<String>("a", 1, vv1)
                )
                .lift(new CausalOperatorO<String>(2,logs))
                .toList().blockingGet();
    }
}
