import chord.hashing.HashingAlgorithm;
import chord.storage.DataStorage;
import chord.storage.Dependencie;
import chord.storage.Version;
import org.junit.jupiter.api.Test;

import java.security.NoSuchAlgorithmException;
import java.util.ArrayList;
import java.util.List;

import static org.junit.Assert.assertEquals;

class VersionTest {

    Version version;
    void setUp() {
        List<Dependencie> dependencies = new ArrayList<>();
        dependencies.add(new Dependencie("key1",0));
        dependencies.add(new Dependencie("key2",1));
        version = new Version(dependencies,"value");
    }

    @Test
    void fromToString() {
        setUp();
        String versionString = version.toString();
        Version parsedVersion = Version.fromStrings(versionString.split("\\|"));
        System.out.println(versionString);
        assertEquals(version,parsedVersion);
    }

    @Test
    void mapfromToString() throws NoSuchAlgorithmException {
        setUp();
        DataStorage dataStorage = new DataStorage(new HashingAlgorithm(1),0);
        List<Dependencie> dependencies = new ArrayList<>();
        dependencies.add(new Dependencie("key2",4));
        dataStorage.insertKey(dependencies,"key1","val1");
        dataStorage.insertKey(dependencies,"key1","val1");
        dataStorage.insertKey(dependencies,"key2","val2");
        String storageString = DataStorage.keysToString(dataStorage.getMap());
        DataStorage dataStorage1 = new DataStorage(new HashingAlgorithm(1),0);
        dataStorage1.insertFromString(storageString);
    }

    @Test
    void mapfromToStringEmpty() throws NoSuchAlgorithmException {
        setUp();
        DataStorage dataStorage = new DataStorage(new HashingAlgorithm(1),0);
        String storageString = DataStorage.keysToString(dataStorage.getMap());
        DataStorage dataStorage1 = new DataStorage(new HashingAlgorithm(1),0);
        dataStorage1.insertFromString(storageString);
    }
}