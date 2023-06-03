package chord;

import java.util.Objects;

public class Finger {
    private final Integer id; // node id
    private final String address; // socket

    public Finger(Integer id, String address) {
        this.id = id;
        this.address = address;
    }

    @Override
    public String toString() {
        return "Finger{" +
                "id=" + id +
                ", address='" + address + '\'' +
                '}';
    }

    @Override
    public boolean equals(Object o) {
        if (this == o) return true;
        if (o == null || getClass() != o.getClass()) return false;
        Finger finger = (Finger) o;
        return Objects.equals(id, finger.id) && Objects.equals(address, finger.address);
    }

    public Integer getId() {
        return id;
    }

    public String getAddress() {
        return address;
    }
}
