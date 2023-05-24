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
    public HashingAlgorithm(int hashFunction) throws NoSuchAlgorithmException {
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

    public Integer hash(String input) {
        byte[] byteArray = messageDigest.digest(input.getBytes());

        // Take the first 4 bytes from the byte array
        byte[] truncatedArray = new byte[4];
        System.arraycopy(byteArray, 0, truncatedArray, 0, 4);

        // Convert the truncated byte array to an integer
        int hashValue = 0;
        for (byte b : truncatedArray) {
            hashValue = (hashValue << 8) | (b & 0xFF);
        }

        return hashValue;
    }

    public Integer hash(Integer input) {
        byte[] byteArray = messageDigest.digest(ByteBuffer.allocate(4).putInt(input).array());

        // Take the first 4 bytes from the byte array
        byte[] truncatedArray = new byte[4];
        System.arraycopy(byteArray, 0, truncatedArray, 0, 4);

        // Convert the truncated byte array to an integer
        int hashValue = 0;
        for (byte b : truncatedArray) {
            hashValue = (hashValue << 8) | (b & 0xFF);
        }

        return hashValue;
    }

}
