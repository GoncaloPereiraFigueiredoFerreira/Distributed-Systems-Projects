package org.example.chord.storage;

import org.example.HashingAlgorithm;
import org.junit.jupiter.api.BeforeEach;
import org.junit.jupiter.api.Test;

import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;

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
}