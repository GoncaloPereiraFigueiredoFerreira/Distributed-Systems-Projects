package observableServer;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.channels.ServerSocketChannel;
import java.nio.charset.StandardCharsets;

public class Server {
    public static void main(String args[]){

        // TODO: Parse the args to retrieve number of nodes


        ServerSocketChannel ss= null;
        try {
            ss = ServerSocketChannel.open();
            ss.bind(new InetSocketAddress(12345));
            var loop = new MainLoop();

            var server = loop.accept(ss);

            // Subscribe method catches the onNext callbacks
            server.subscribe(conn -> {
                var coisas = loop.read(conn);
                //coisas.lift(new CausalOperator<String>(2))
                coisas.map(bb -> StandardCharsets.UTF_8.decode(bb))
                        .subscribe(s -> System.out.println(s));
            });
            loop.run();

            /*
            var in = ... // Mensagens desordenadas
                         // da classe CausalMessage<T>
            in.lift(new CausalOperator<T>(n)) // Ordenação
                    .map(payload -> process(payload)) // Processamento de mensagens
                         // ordenadas da classe T
                    .subscribe();
            */
        } catch (IOException e) {
            e.printStackTrace();
        }

    }
}

