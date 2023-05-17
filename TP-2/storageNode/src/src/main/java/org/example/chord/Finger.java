package org.example.chord;

public class Finger {
    private final int id; // node id
    private final String hostname; // socket
    private final int port; // socket

    public Finger(int id, String hostname, int port) {
        this.id = id;
        this.hostname = hostname;
        this.port = port;
    }

    public int getId() {
        return id;
    }

    public String getHostname() {
        return hostname;
    }


    public int getPort() {
        return port;
    }
}
