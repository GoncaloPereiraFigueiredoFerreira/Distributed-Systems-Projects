import chord.NodeController;

public class Main {
    public static void main(String[] args) {
        NodeController loadBalancer = new NodeController(30,5);
        loadBalancer.run();
    }
}