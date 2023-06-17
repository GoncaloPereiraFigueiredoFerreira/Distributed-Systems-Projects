package chord;

public class NodeEntry {
    private String addressDest;
    private Integer idDest;
    private int id;

    public NodeEntry(String addressDest, Integer idDest, int id) {
        this.addressDest = addressDest;
        this.idDest = idDest;
        this.id = id;
    }

    public String getAddressDest() {
        return addressDest;
    }

    public Integer getIdDest() {
        return idDest;
    }

    public int getId() {
        return id;
    }
}
