import chord.Node;
import chord.NodeController;
import org.zeromq.*;

import java.util.Objects;

public class Main {
    public static void main(String[] args) {
        NodeController loadBalancer = new NodeController(5,5);
        loadBalancer.run();
    }
}