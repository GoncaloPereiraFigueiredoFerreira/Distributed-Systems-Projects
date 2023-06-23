import chord.NodeController;

public class Main {
    public static void main(String[] args) {
        NodeController nodeController = new NodeController(10,4);
        nodeController.run();
    }
}