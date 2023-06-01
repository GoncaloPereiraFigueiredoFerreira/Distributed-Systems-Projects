package org.example.chord.storage;

import org.example.HashingAlgorithm;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;
import java.util.Map;

import static org.junit.jupiter.api.Assertions.*;

class VersionTest {

    Version version;
    @BeforeEach
    void setUp() {
        List<Dependencie> dependencies = new ArrayList<>();
        dependencies.add(new Dependencie("key1",0));
        dependencies.add(new Dependencie("key2",1));
        version = new Version(dependencies,"value");
    }

    @Test
    void fromToString() {
        String versionString = version.toString();
        Version parsedVersion = Version.fromStrings(versionString.split("\\|"));
        System.out.println(versionString);
        assertEquals(version,parsedVersion);
    }

    @Test
    void mapfromToString() throws NoSuchAlgorithmException {
        DataStorage dataStorage = new DataStorage(new HashingAlgorithm(1));
        List<Dependencie> dependencies = new ArrayList<>();
        dependencies.add(new Dependencie("key1",0));
        dependencies.add(new Dependencie("key2",1));
        dataStorage.insertKey(dependencies,"key1","val1");
        dataStorage.insertKey(dependencies,"key1","val1");
        dataStorage.insertKey(dependencies,"key2","val2");
        String storageString = dataStorage.toString();
        DataStorage dataStorage1 = new DataStorage(new HashingAlgorithm(1));
        dataStorage1.insertFromString(storageString);
    }

    @Test
    void mapfromToStringEmpty() throws NoSuchAlgorithmException {
        DataStorage dataStorage = new DataStorage(new HashingAlgorithm(1));
        String storageString = DataStorage.keysToString(dataStorage.getMap());
        DataStorage dataStorage1 = new DataStorage(new HashingAlgorithm(1));
        dataStorage1.insertFromString(storageString);
    }
}