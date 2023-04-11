package observableServer;

import causalop.*;
import io.reactivex.rxjava3.annotations.NonNull;
import io.reactivex.rxjava3.core.Observable;
import io.reactivex.rxjava3.observables.GroupedObservable;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.util.ArrayList;
import java.util.List;

public class Server extends Thread {
    int nServers;
    int identifier;
    ServerSocketChannel ss;
    List<SocketChannel> channels;

    public Server(int identifier,int nServers,ServerSocketChannel ss,List<SocketChannel> channels){
        this.nServers=nServers;
        this.identifier=identifier;
        this.ss=ss;
        this.channels=channels;
    }

    public void run() {
        try {
            var loop = new MainLoop();
            var server = loop.accept(ss);

            // Subscribe method catches the onNext callbacks
            CausalOperator co = new CausalOperator(nServers);
            server.subscribe(conn -> {
                @NonNull Observable<GroupedObservable<Integer, Message>> in =
                        loop.read(conn)
                            .lift(new CausalMessageReader())
                            .groupBy(Message::getType);

               in.subscribe(group -> {
                    if (group.getKey() == 0) { //CBCast messages
                        group.map(message -> (CausalMessage) message)
                                .lift(co)
                                .subscribe(s -> System.out.println("received: " + s));
                    } else if (group.getKey() == 1) { //Client messages
                        group.map(message -> (ClientMessage) message)
                                .map(message -> new CausalMessage<>(message.getContent(),identifier,co.getAndIncrementVV(identifier)))
                                .subscribe(message -> {
                                    for(SocketChannel channel:channels){
                                        channel.write(message.toByteBuffer());
                                    }
                                    //conn.write(ByteBuffer.wrap()); //TODO escrever de volta ao cliente
                                });
                    } else {
                        throw new IllegalArgumentException("Unsupported message type");
                    }
                });
            });
            loop.run();

        } catch (IOException e) {
            e.printStackTrace();
        }

    }
}

