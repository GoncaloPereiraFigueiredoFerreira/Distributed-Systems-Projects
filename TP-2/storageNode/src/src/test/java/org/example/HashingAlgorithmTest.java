package org.example;

import org.junit.Assert;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.math.BigInteger;
import java.security.NoSuchAlgorithmException;

import static org.junit.jupiter.api.Assertions.*;

class HashingAlgorithmTest {
    HashingAlgorithm hashingAlgorithm;
    @BeforeEach
    void setUp() throws NoSuchAlgorithmException {
        hashingAlgorithm= new HashingAlgorithm(1);
    }

    @Test
    void hash() {
        Integer val1 =  hashingAlgorithm.hash("server1");
        Integer val2 =  hashingAlgorithm.hash("server2");
        assertNotEquals(val1, val2);
    }
}