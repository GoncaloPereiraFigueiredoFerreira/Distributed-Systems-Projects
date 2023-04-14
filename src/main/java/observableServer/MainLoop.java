package observableServer;

import io.reactivex.rxjava3.core.Observable;
import io.reactivex.rxjava3.core.ObservableEmitter;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.channels.SelectionKey;
import java.nio.channels.Selector;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.nio.channels.spi.SelectorProvider;
import java.util.Iterator;

public class MainLoop {
    Selector sel;
    public MainLoop() throws IOException {
        // Asks the SO for a Selector
        this.sel = SelectorProvider.provider().openSelector();
    }

    //Returns an observable socket channel when you accept a new socket
    public Observable<SocketChannel> accept(ServerSocketChannel ss){
        return Observable.create(sub->{
            ss.configureBlocking(false);
            //Register the server socket in the Selector for reception of new socket channels
            ss.register(sel, SelectionKey.OP_ACCEPT, sub);
        });
    }

    // Returns an observable byte buffer read from the socket channel s
    public Observable<ByteBuffer> read(SocketChannel s) {
        return Observable.create(sub -> {
            s.configureBlocking(false);
            // Register the Socket Channel for possible read socket
            s.register(sel, SelectionKey.OP_READ, sub);
        });
    }



    public void run() throws IOException {
        while(true) {
            sel.select(); //blocks until accept is performed

            for (Iterator<SelectionKey> i = sel.selectedKeys().iterator(); i.hasNext(); ) {
                SelectionKey key = i.next();
                if (key.isAcceptable()) {
                    ServerSocketChannel ss = (ServerSocketChannel) key.channel();
                    SocketChannel s = ss.accept();
                    var sub = (ObservableEmitter<SocketChannel>) key.attachment();
                    sub.onNext(s); // Call back for the emition of the accepted socket channel
                    key.attach(sub);
                }
                else if (key.isReadable()){
                    var sub = (ObservableEmitter<ByteBuffer>) key.attachment();
                    var s = (SocketChannel)key.channel();
                    var bb = ByteBuffer.allocate(1000);
                    while(true) {
                        bb.clear();
                        int readNumbers = s.read(bb);
                        if (readNumbers <= 0) {
                            break;
                        }
                        bb.flip();
                        sub.onNext(bb.duplicate());
                    }
                    key.attach(sub);
                }
                i.remove();
            }
        }
    }
}

