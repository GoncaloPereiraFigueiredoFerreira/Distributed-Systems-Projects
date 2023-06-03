import chord.hashing.HashingAlgorithm;
import org.junit.jupiter.api.Test;

import java.security.NoSuchAlgorithmException;

import static org.junit.jupiter.api.Assertions.*;

class HashingAlgorithmTest {
    HashingAlgorithm hashingAlgorithm;

    void setUp() throws NoSuchAlgorithmException {
        hashingAlgorithm= new HashingAlgorithm(1);
    }

    @Test
    void hash() throws NoSuchAlgorithmException {
        setUp();
        Integer val1 =  hashingAlgorithm.hash("server1");
        Integer val2 =  hashingAlgorithm.hash("server2");
        assertNotEquals(val1, val2);
    }
}