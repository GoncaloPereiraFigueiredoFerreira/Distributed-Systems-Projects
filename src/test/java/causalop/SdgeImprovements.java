package causalop;

import io.reactivex.rxjava3.core.Flowable;
import org.junit.Assert;
import org.junit.Test;

import java.util.*;

public class SdgeImprovements {
    VersionVector2 vvi;
    VersionVector2 vvj;
    VersionVector2 vvk;

    public void setup(){
        vvi = new VersionVector2(3);
        vvj = new VersionVector2(3);
        vvk = new VersionVector2(3);
    }

    private boolean sameMap(Map<Integer,Integer> m1,Map<Integer,Integer> m2) {
        return m1.entrySet().equals(m2.entrySet());
    }

    @Test
    public void noDependencies() {
        setup(); // all vv's start with 0's
        CausalOperatorF cof = new CausalOperatorF<>(3);
        var l = Flowable.just(
                        new CausalMessage<>(null, 1, vvj.cbcast(1,new ArrayList<>()))
                )
                .lift(cof)
                .toList().blockingGet();

        Map<Integer,Integer> expectedResult = new HashMap<>();
        expectedResult.put(0,2);
        cof.cbCast(0);
        VersionVector2 result = cof.cbCast(0);
        Assert.assertTrue(sameMap(expectedResult,result.getVV()));
    }

    @Test
    public void oneDependencie() {
        setup(); // all vv's start with 0's
        vvk = new VersionVector2(new int[]{0, 1, 0}); //k will send message after receiving from j
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
        VersionVector2 result = cof.cbCast(0);
        Assert.assertTrue(sameMap(expectedResult,result.getVV()));
    }

    @Test
    public void twoDependencies() {
        setup(); // all vv's start with 0's
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
        VersionVector2 result = cof.cbCast(0);
        Assert.assertTrue(sameMap(expectedResult,result.getVV()));
    }
}
