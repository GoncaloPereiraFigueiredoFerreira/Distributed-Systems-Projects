package observableServer;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.logging.Logger;
import java.util.stream.Collectors;

public class ServerStarter {
    public static void main(String[] args) throws IOException {
        int nServers = 5;
        Logger logs = Logger.getLogger(ServerStarter.class.getName());

        Map<Integer, ServerSocketChannel> serverSockets = new HashMap<>();
        Map<Integer, Map<Integer,SocketChannel>> sockets = new HashMap<>();

        for(int j=0;j<nServers;j++){
            ServerSocketChannel ss = ServerSocketChannel.open();
            ss.bind(new InetSocketAddress(12340+j));
            ss.configureBlocking(false);
            serverSockets.put(j,ss);
        }

        for(int j=0;j<nServers;j++){
            sockets.put(j,new HashMap<>());
        }

        for(int j=0;j<nServers;j++){
            Map<Integer,SocketChannel> mapaOrigem = sockets.get(j);
            for (int i=j+1;i<nServers;i++){
                SocketChannel channel = SocketChannel.open();
                channel.connect(new InetSocketAddress("localhost", 12340+i));
                mapaOrigem.put(i,channel);
            }
        }

        Map<Integer, List<SocketChannel>> transformedMap = sockets.entrySet().stream()
                .collect(Collectors.toMap(
                        Map.Entry::getKey,
                        e -> e.getValue().entrySet().stream()
                                .sorted(Map.Entry.comparingByKey())
                                .map(Map.Entry::getValue)
                                .collect(Collectors.toList())));


        for(int identifier=0;identifier<nServers;identifier++){
            Server server = new Server(identifier,
                    nServers,
                    serverSockets.get(identifier),
                    transformedMap.get(identifier),
                    logs);
            server.start();
        }
    }
}
