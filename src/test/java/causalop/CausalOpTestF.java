package causalop;

import io.reactivex.rxjava3.core.Flowable;
import org.junit.Assert;
import org.junit.Test;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;


public class CausalOpTestF {
    VersionVector2 vv1;
    VersionVector2 vv2;
    VersionVector2 vv3;
    VersionVector2 vv4;

    public void setup(){
        vv1 = new VersionVector2(new int[]{0, 1});
        vv2 = new VersionVector2(new int[]{1, 0});
        vv3 = new VersionVector2(new int[]{1, 2});
    }



    @Test
    public void testOk() {
        setup();
        var l = Flowable.just(
                        new CausalMessage<>("a", 1, vv1),
                        new CausalMessage<>("b", 0, vv2),
                        new CausalMessage<>("c", 1, vv3)
                )
                .lift(new CausalOperatorF<>(2))
                .toList().blockingGet();

        Assert.assertArrayEquals(l.toArray(), new String[]{"a","b","c"});
    }

    @Test
    public void testReorder() {
        setup();
        var l = Flowable.just(
                        new CausalMessage<>("c", 1, vv3),
                        new CausalMessage<>("a", 1, vv1),
                        new CausalMessage<>("b", 0, vv2)
                )
                .lift(new CausalOperatorF<>(2))
                .toList().blockingGet();

        Assert.assertArrayEquals(l.toArray(), new String[]{"a","b","c"});
    }

    @Test
    public void testDupl() {
        setup();
        var l = Flowable.just(
                        new CausalMessage<>("a", 1, vv1),
                        new CausalMessage<>("b", 0, vv2),
                        new CausalMessage<>("a", 1, vv1),
                        new CausalMessage<>("c", 1, vv3)
                )
                .lift(new CausalOperatorF<>(2))
                .toList().blockingGet();

        Assert.assertArrayEquals(l.toArray(), new String[]{"a","b","c"});
    }

    @Test(expected = IllegalArgumentException.class)
    public void testGap() {
        setup();
        var l = Flowable.just(
                        new CausalMessage<>("c", 1, vv3),
                        new CausalMessage<>("a", 1, vv1)
                )
                .lift(new CausalOperatorF<>(2))
                .toList().blockingGet();
    }
}
