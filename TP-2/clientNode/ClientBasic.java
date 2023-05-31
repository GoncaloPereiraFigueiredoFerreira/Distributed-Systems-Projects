import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.channels.SocketChannel;
import java.util.ArrayList;
import java.util.Map;

public class ClientBasic {

    public static void main(String[] args){
        InputMenu login = new InputMenu("Please specify your login name","Username");
        String[] sessionServers = new String[]{"Server1","Server2","Server3"};
        Menu chooseSession = new Menu("Please specify to which server do you wish to connect:",sessionServers);
        String[] actions = new String[]{"Write a key-value", "Read values"};
        Menu chooseAction = new Menu("Choose an action",actions);
        InputMenu keyInput = new InputMenu("Input a key","key");
        InputMenu valueInput = new InputMenu("Input a value", "value");

        while (true){
            login.execute();
            String user = login.getOption();
            chooseSession.executa();
            if (chooseSession.getOption() != 0){
                try {
                    ClientOperations co = new ClientOperations(user,InetSocketAddress.createUnresolved("localhost",30));
                    while(chooseAction.getOption()!=-1) {
                        chooseAction.executa();
                        switch (chooseAction.getOption()) {
                            case 1 -> {
                                // Write
                                keyInput.execute();
                                valueInput.execute();
                                if (keyInput.getOption().length()>0 && valueInput.getOption().length()>0){
                                    System.out.println("Sent the write request...");
                                    co.writeValue(keyInput.getOption(), valueInput.getOption());
                                    System.out.println("Key-value pair written!");
                                }
                            }
                            case 2 -> {
                                // Read
                                ArrayList<String> keys = new ArrayList<>();
                                do{
                                    System.out.println("Input as many as you would like, and then hit enter");
                                    keyInput.execute();
                                    if (keyInput.getOption().length()>0) keys.add(keyInput.getOption());
                                }while(keyInput.getOption().length()!=0);
                                if (keys.size()>0){
                                    System.out.println("Sent the read request...");
                                    String[] arr = new String[keys.size()];
                                    Map<String,String> read = co.readNValues(keys.toArray(arr));
                                }
                            }
                        }
                    }
                } catch (IOException e) {
                    System.out.println("Unable to connect to server!");
                }
            }
        }
    }
}
