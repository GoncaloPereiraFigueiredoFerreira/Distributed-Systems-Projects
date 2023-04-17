package causalop;

import io.reactivex.rxjava3.core.Flowable;
import org.junit.Assert;
import org.junit.Test;

import java.util.HashMap;
import java.util.Map;
import java.util.Objects;


public class CausalOpTestF {
    VersionVector vv1;
    VersionVector vv2;
    VersionVector vv3;
    VersionVector vv4;

    public void setup(){
        vv1 = new VersionVector(new int[]{0, 1});
        vv2 = new VersionVector(new int[]{1, 0});
        vv3 = new VersionVector(new int[]{1, 2});
    }

    public void setup2(){
        vv1 = new VersionVector(new int[]{4, 3, 7});
        vv2 = new VersionVector(new int[]{4, 4, 7});
    }

    public void setup3(){
        vv1 = new VersionVector(new int[]{5, 3, 7});
        vv2 = new VersionVector(new int[]{5, 4, 7});
        vv3 = new VersionVector(new int[]{5, 4, 8});
        vv4 = new VersionVector(new int[]{6, 4, 8});
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

    private boolean sameMap(Map<Integer,Integer> m1,Map<Integer,Integer> m2) {
        if(m1.size()!=m2.size()){
            return false;
        }
        for (Map.Entry<Integer,Integer> entry:m1.entrySet()){
            if(!Objects.equals(m2.get(entry.getKey()), entry.getValue())){
                return false;
            }
        }
        return true;
    }

    @Test
    public void sdgeImprv1() {
        setup2();
        CausalOperatorF<String> co = new CausalOperatorF<>(3,vv1);
        vv1=co.cbCast(0);

        var l = Flowable.just(
                        new CausalMessage<>("", 1, vv2)
                )
                .lift(co)
                .subscribe();
        Map<Integer,Integer> result = new HashMap<>();
        result.put(0,6);
        result.put(1,4);
        Assert.assertTrue(sameMap(co.cbCast(0).getVV(),result));
    } //TODO permitir comparações com vetores mais pequenos

    @Test
    public void sdgeImprv2() {
        setup3();
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
