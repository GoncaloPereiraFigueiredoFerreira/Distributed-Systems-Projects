package chord;

import chord.hashing.HashingAlgorithm;
import org.zeromq.*;

import java.security.NoSuchAlgorithmException;
import java.util.*;

public class NodeController implements Runnable{
    TreeMap<Integer, String> allIds = new TreeMap<>();
    HashSet<Integer> addingNodes = new HashSet<>();
    int basePort;
    int counter = 0;
    String loadBalancerAddress;

    private HashingAlgorithm hashingAlgorithm;
    private int nExtraStartingNodes;
    private int nReplicas;

    public NodeController(int nExtraStartingNodes, int nReplicas){
        this.nExtraStartingNodes=nExtraStartingNodes;
        this.nReplicas=nReplicas;
    }

    @Override
    public void run() {
        basePort = 5555;
        loadBalancerAddress = "tcp://localhost:5550";
        try {
            hashingAlgorithm= new HashingAlgorithm(1);
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException(e);
        }

        startChordNode(true);

        Thread nodesStarter = new Thread(()-> {
            for (int i = 0; i < nExtraStartingNodes; i++) {
                while (addingNodes.size() > 0) {
                    try {
                        Thread.sleep(50);
                    } catch (InterruptedException e) {
                        throw new RuntimeException(e);
                    }
                }
                startChordNode(false);
            }
        });
        nodesStarter.start();

        try (ZContext context = new ZContext()) {
            ZMQ.Socket frontend = context.createSocket(SocketType.REP);
            frontend.bind(loadBalancerAddress);

            while (!Thread.currentThread().isInterrupted()) {
                processRequest(frontend);
            }
        }
    }

    public void processRequest(ZMQ.Socket socket) {
        String cont = socket.recvStr(0);
        String[] values = cont.split("\\|");
        if(Objects.equals(values[0], "")||Objects.equals(values[0], "null"))
            values = Arrays.copyOfRange(values, 1, values.length);
        String returnMessage = processMessageContent(values);
        socket.send(returnMessage);
    }

    private String processMessageContent(String[] values)   {
        switch (values[0]) {
            case "join_completed" -> {
                System.out.println("JoinComplete from: "+ values[1]);
                int nodeId = Integer.parseInt(values[1]);
                addingNodes.remove(nodeId);
                return "ACK";
            }
            case "add_node" -> {
                System.out.println("New node request");
                if(addingNodes.size()>0){
                    return "NACK";
                } else {
                    startChordNode(false);
                    return "ACK";
                }
            }
            default -> {
                System.out.println("Received invalid request");
                return null;
            }
        }
    }
    public void startChordNode(Boolean firstExecution)  {
        String address = "tcp://localhost:" + basePort;
        List<NodeEntry> entrys = generateReplicaEntrys(address,firstExecution);
        Thread thread;
        try {
            thread = new Thread(new NodeRunner(firstExecution,address, entrys,loadBalancerAddress));
        } catch (NoSuchAlgorithmException e) {
            throw new RuntimeException(e);
        }
        thread.start();

        basePort+=4;
    }

    private List<NodeEntry> generateReplicaEntrys(String address,Boolean firstExecution){
        List<NodeEntry> entrys = new ArrayList<>();
        Set<Integer> Ids = allIds.keySet();
        for (int i = 0; i <nReplicas; i++) {
            Integer key = hashingAlgorithm.hash(address + i);
            while (Ids.contains(key)||entrys.stream().map(NodeEntry::getId).toList().contains(key)){
                key = hashingAlgorithm.hash(address + i + counter++);
            }

            Integer precedingNode = getClosestPrecedingNode(key);
            if(precedingNode==null)
                entrys.add(new NodeEntry(address,null,key));
            else entrys.add(new NodeEntry(allIds.get(precedingNode),precedingNode,key));
            if(!(firstExecution&&i==0))
                addingNodes.add(key);
        }
        for (Integer newId:entrys.stream().map(NodeEntry::getId).toList()){
            allIds.put(newId,address);
        }
        return entrys;
    }
    public Integer getClosestPrecedingNode(int targetKey) {
        if(allIds.size()==0)
            return null;
        Integer precedingKey = allIds.lowerKey(targetKey);
        if (precedingKey == null) {
            precedingKey = allIds.lastKey();
        }
        return precedingKey;
    }
    //private closestNode()
}
