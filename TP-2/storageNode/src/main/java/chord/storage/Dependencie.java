package chord.storage;

import java.util.Objects;

public class Dependencie {
    private final String key;
    private final int version;

    public Dependencie(String key, int version) {
        this.key = key;
        this.version = version;
    }

    public String getKey() {
        return key;
    }

    public int getVersion() {
        return version;
    }

    public String toString(){
        return key+"|"+version;
    }

    public static Dependencie fromString(String input) {

        String[] parts = input.split("\\|");

        String key = parts[0];
        int version = Integer.parseInt(parts[1]);

        return new Dependencie(key, version);
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Dependencie that = (Dependencie) o;
        return version == that.version && Objects.equals(key, that.key);
    }
}
