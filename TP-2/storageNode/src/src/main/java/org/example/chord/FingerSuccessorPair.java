package org.example.chord;

public class FingerSuccessorPair {
    private final Finger finger;
    private final boolean found;

    public FingerSuccessorPair(Finger finger, boolean found) {
        this.finger = finger;
        this.found = found;
    }

    public Finger getFinger() {
        return finger;
    }

    public boolean isFound() {
        return found;
    }
}
