package causalop;

import io.reactivex.rxjava3.core.Flowable;
import org.junit.Assert;
import org.junit.Test;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;


public class CausalOpTestF {
    VersionVector vv1;
    VersionVector vv2;
    VersionVector vv3;
    VersionVector vvi;
    VersionVector vvj;
    VersionVector vvk;

    public void setup(){
        vv1 = new VersionVector(new int[]{0, 1});
        vv2 = new VersionVector(new int[]{1, 0});
        vv3 = new VersionVector(new int[]{1, 2});
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


    public void setup2(){
        vvi = new VersionVector(3);
        vvj = new VersionVector(3);
        vvk = new VersionVector(3);
    }

    private boolean sameMap(Map<Integer,Integer> m1, Map<Integer,Integer> m2) {
        return m1.entrySet().equals(m2.entrySet());
    }

    @Test
    public void noDependencies() {
        setup2(); // all vv's start with 0's
        CausalOperatorF cof = new CausalOperatorF<>(3);
        var l = Flowable.just(
                        new CausalMessage<>(null, 1, vvj.cbcast(1,new ArrayList<>()))
                )
                .lift(cof)
                .toList().blockingGet();

        Map<Integer,Integer> expectedResult = new HashMap<>();
        expectedResult.put(0,2);
        cof.cbCast(0);
        VersionVector result = cof.cbCast(0);
        Assert.assertTrue(sameMap(expectedResult,result.getVV()));
    }

    @Test
    public void oneDependencie() {
        setup2(); // all vv's start with 0's
        vvk = new VersionVector(new int[]{0, 1, 0}); //k will send message after receiving from j
        List<Integer> vvkDependencies = new ArrayList<>();
        vvkDependencies.add(1);

        CausalOperatorF cof = new CausalOperatorF<>(3);
        var l = Flowable.just(
                        new CausalMessage<>(null, 1, vvj.cbcast(1,new ArrayList<>())), //No dependencies
                        new CausalMessage<>(null, 2, vvk.cbcast(2,vvkDependencies)) //Has dependencie from j
                )
                .lift(cof)
                .toList().blockingGet();

        Map<Integer,Integer> expectedResult = new HashMap<>();
        expectedResult.put(0,1);
        expectedResult.put(2,1);
        VersionVector result = cof.cbCast(0);
        Assert.assertTrue(sameMap(expectedResult,result.getVV()));
    }

    @Test
    public void twoDependencies() {
        setup2(); // all vv's start with 0's
        CausalOperatorF cof = new CausalOperatorF<>(3);
        var l = Flowable.just(
                        new CausalMessage<>(null, 1, vvj.cbcast(1,new ArrayList<>())), //No dependencies
                        new CausalMessage<>(null, 2, vvk.cbcast(2,new ArrayList<>())) //No dependencies
                )
                .lift(cof)
                .toList().blockingGet();

        Map<Integer,Integer> expectedResult = new HashMap<>();
        expectedResult.put(0,1);
        expectedResult.put(1,1);
        expectedResult.put(2,1);
        VersionVector result = cof.cbCast(0);
        Assert.assertTrue(sameMap(expectedResult,result.getVV()));
    }

    @Test
    public void bcastTwice() {
        setup2(); // all vv's start with 0's
        vvk = new VersionVector(new int[]{0, 1, 0}); //k will send message after receiving from j
        List<Integer> vvkDependencies = new ArrayList<>();
        vvkDependencies.add(1);

        CausalOperatorF cof = new CausalOperatorF<>(3);
        var l = Flowable.just(
                        new CausalMessage<>(null, 1, vvj.cbcast(1,new ArrayList<>())), //No dependencies
                        new CausalMessage<>(null, 2, vvk.cbcast(2,vvkDependencies)), //Has dependencie from j
                        new CausalMessage<>(null, 1, vvj.cbcast(1,new ArrayList<>())) //Depends on itself
                )
                .lift(cof)
                .toList().blockingGet();

        Map<Integer,Integer> expectedResult = new HashMap<>();
        expectedResult.put(0,1);
        expectedResult.put(1,2);
        expectedResult.put(2,1);
        VersionVector result = cof.cbCast(0);
        Assert.assertTrue(sameMap(expectedResult,result.getVV()));
    }
}
