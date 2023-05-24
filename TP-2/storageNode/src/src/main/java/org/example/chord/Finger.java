package org.example.chord;

public class Finger {
    private final int id; // node id
    private final String address; // socket

    public Finger(int id, String address) {
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

    public int getId() {
        return id;
    }

    public String getAddress() {
        return address;
    }
}
