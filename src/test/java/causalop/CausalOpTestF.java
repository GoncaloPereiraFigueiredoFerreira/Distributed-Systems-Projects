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
        vv1 = new VersionVector2(new int[]{4, 3, 7});
        vv2 = new VersionVector2(new int[]{4, 4, 7});
        HashMap map = new HashMap<>();
        map.put(2,8);
        vv3 = new VersionVector2(map);


        CausalOperatorF<String> co = new CausalOperatorF<>(3,vv1);
        vv1=co.cbCast(0);

        var l = Flowable.just(
                        new CausalMessage<>("", 1, vv2),
                        new CausalMessage<>("", 2, vv3)
                )
                .lift(co)
                .subscribe();
        Map<Integer,Integer> result = new HashMap<>();
        result.put(0,6);
        result.put(2,8);
        Assert.assertTrue(sameMap(co.cbCast(0).getVV(),result));
    }
}
