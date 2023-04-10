package observableServer;

import causalop.CausalMessageReader;
import causalop.CausalOperator;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.channels.ServerSocketChannel;

public class Server {
    public static void main(String args[]) throws IOException {

        // TODO: Parse the args to retrieve number of nodes

        int n = 5;
        //CBCast<String> cbCast = new CBCast<>(n,0);
        ServerSocketChannel ss= null;
        try {
            ss = ServerSocketChannel.open();
            ss.bind(new InetSocketAddress(12345));
            var loop = new MainLoop();

            var server = loop.accept(ss);

            // Subscribe method catches the onNext callbacks
            CausalOperator co = new CausalOperator(n);
            server.subscribe(conn -> {
                var in = loop.read(conn)
                            .lift(new CausalMessageReader())
                            .lift(co)
                            .subscribe(s -> System.out.println("received: " + s));
                //identificar a mensagem
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

