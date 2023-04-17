package observableServer;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.util.HashMap;
import java.util.Map;
import java.util.logging.Logger;
import java.util.stream.Collectors;

public class ServerStarter {
    public static void main(String[] args) throws IOException {
        int nServers = 5;
        Logger logs = Logger.getLogger(ServerStarter.class.getName());

        Map<Integer,ServerSocketChannel> serverSockets = new HashMap<>();
        Map<Integer,SocketChannel> sockets = new HashMap<>();

        for(int j=0;j<nServers;j++){
            ServerSocketChannel ss = ServerSocketChannel.open();
            ss.bind(new InetSocketAddress(12340+j));
            serverSockets.put(j,ss);
        }

        for(int j=0;j<nServers;j++){
            SocketChannel channel = SocketChannel.open();
            channel.connect(new InetSocketAddress("localhost", 12340+j));
            sockets.put(j,channel);
        }

        for(int identifier=0;identifier<nServers;identifier++){
            int finalIdentifier = identifier;
            Server server = new Server(finalIdentifier,
                                        nServers,
                                        serverSockets.get(finalIdentifier),
                                        sockets.entrySet().stream().filter(e -> e.getKey()!= finalIdentifier).map(Map.Entry::getValue).collect(Collectors.toList()),
                                        logs);
            server.start();
        }
    }
}
