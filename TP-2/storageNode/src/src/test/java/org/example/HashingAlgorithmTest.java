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
        hashingAlgorithm= new HashingAlgorithm(1,1);
    }

    @Test
    void hash() {
        BigInteger val1 =  hashingAlgorithm.hash(1);
        BigInteger val2 =  hashingAlgorithm.hash(2);
        assertEquals(val1, val2);
    }
}