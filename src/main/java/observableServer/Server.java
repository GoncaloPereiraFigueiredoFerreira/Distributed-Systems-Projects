package observableServer;

import causalop.*;
import io.reactivex.rxjava3.annotations.NonNull;
import io.reactivex.rxjava3.core.Observable;
import io.reactivex.rxjava3.observables.GroupedObservable;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.nio.channels.spi.SelectorProvider;
import java.util.ArrayList;
import java.util.List;
import java.util.logging.Level;
import java.util.logging.Logger;

public class Server extends Thread {
    int nServers;
    int identifier;
    ServerSocketChannel ss;
    List<SocketChannel> channels;
    Logger logger;

    public Server(int identifier, int nServers, ServerSocketChannel ss, List<SocketChannel> channels, Logger logger){
        this.nServers=nServers;
        this.identifier=identifier;
        this.ss=ss;
        this.channels=channels;
        this.logger=logger;
    }

    public void run() {
        try {


            var loop = new MainLoop();
            var server = loop.accept(ss);

            // Subscribe method catches the onNext callbacks
            CausalOperator co = new CausalOperator(nServers,logger);
            /*
            for(SocketChannel channel:channels){
                @NonNull Observable<GroupedObservable<Integer, Message>> in = loop.read(channel)
                                                                                  .lift(new CausalMessageReader())
                                                                                  .groupBy(Message::getType);
                in.subscribe(group -> {
                    group.map(message -> (CausalMessage) message)
                        .lift(co)
                        .subscribe(s -> logger.log(Level.INFO,"received: " + s));
                });
            }
*/
            server.subscribe(conn -> {
                @NonNull Observable<GroupedObservable<Integer, Message>> in =
                        loop.read(conn)
                            .lift(new CausalMessageReader())
                            .groupBy(Message::getType);

               in.subscribe(group -> {
                    if (group.getKey() == 0) { //CBCast messages

                        group.map(message -> (CausalMessage) message)
                                .lift(co)
                                .subscribe(s -> {
                                    logger.log(Level.INFO,identifier+"received causal msg: " + s);
                                });
                    } else if (group.getKey() == 1) { //Client messages
                        group.map(message -> (ClientMessage) message)
                                .map(message -> new CausalMessage<>(message.getContent(),identifier,co.getAndIncrementVV(identifier)))
                                .subscribe(message -> {
                                    logger.log(Level.INFO,identifier+"received client msg: " + message.payload);
                                    for(int j=0;j<nServers;j++){
                                        SocketChannel channel = SocketChannel.open();
                                        channel.connect(new InetSocketAddress("localhost", 12340+j));
                                        ByteBuffer buffer = message.toByteBuffer();
                                        channel.write(buffer);
                                        channel.close();
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

