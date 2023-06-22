import chord.NodeController;

public class Main {
    public static void main(String[] args) {
        NodeController loadBalancer = new NodeController(10,4);
        loadBalancer.run();
    }
}