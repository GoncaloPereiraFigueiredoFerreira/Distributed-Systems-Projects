package org.example;

import java.math.BigInteger;
import java.nio.ByteBuffer;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;

/*
Hashing functions:

1- SHA-224, with 224-bit hash values
2- SHA-256, with 256-bit hash values
3- SHA-384, with 384-bit hash values
4- SHA-512, with 512-bit hash values
5- SHA-512/224, with 512-bit hash values
6- SHA-512/256, with 512-bit hash values
 */
public class HashingAlgorithm {
    private final MessageDigest messageDigest;
    private int m;
    public HashingAlgorithm(int m, int hashFunction) throws NoSuchAlgorithmException {
        this.m = m;
        switch (hashFunction){
            case 1:
                messageDigest = MessageDigest.getInstance("SHA-224");
                break;
            case 2:
                messageDigest = MessageDigest.getInstance("SHA-256");
                break;
            case 3:
                messageDigest = MessageDigest.getInstance("SHA-384");
                break;
            case 4:
                messageDigest = MessageDigest.getInstance("SHA-512");
                break;
            case 5:
                messageDigest = MessageDigest.getInstance("SHA-512/224");
                break;
            case 6:
                messageDigest = MessageDigest.getInstance("SHA-512/256");
                break;
            default:
                throw new NoSuchAlgorithmException();
        }
    }

    public BigInteger hash(Integer input){
        // digest() method is called
        // to calculate message digest of the input string
        // returned as array of byte
        byte[] byteArray = messageDigest.digest(ByteBuffer.allocate(m).putInt(input).array());

        // Convert byte array into signum representation
        BigInteger no = new BigInteger(1, byteArray);

        // return the HashText
        return no;
    }
}
