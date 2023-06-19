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
        System.out.println(hashingAlgorithm.hash("baguete"));
    }
}