import java.io.IOException;
import java.net.InetSocketAddress;
import java.nio.channels.SocketChannel;
import java.util.ArrayList;
import java.util.Map;

public class ClientBasic {

    public static void main(String[] args){
        InputMenu login = new InputMenu("Please specify your login name","Username");
        String[] sessionServers = new String[]{"Server1","Server2","Server3"};
        InetSocketAddress[] sessionServersAddress = new InetSocketAddress[]{new InetSocketAddress("0.0.0.0",12345),new InetSocketAddress("0.0.0.0",12346),new InetSocketAddress("0.0.0.0",12347)};
        Menu chooseSession = new Menu("Please specify to which server do you wish to connect:",sessionServers);
        String[] actions = new String[]{"Write a key-value", "Read values", "Add new data server","Logout"};
        Menu chooseAction = new Menu("Choose an action",actions);
        InputMenu keyInput = new InputMenu("Input a key","key");
        InputMenu valueInput = new InputMenu("Input a value", "value");

        while (true){
            login.execute();
            String user = login.getOption();
            chooseSession.executa();
            if (chooseSession.getOption() != 0){
                try {
                    ClientOperations co = new ClientOperations(sessionServersAddress[chooseSession.getOption()]);
                    co.login(user);
                    boolean logged = true;
                    while(chooseAction.getOption()!=-1 && logged) {
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
                                    if (read != null) {
                                        System.out.println(read);
                                    }else {System.out.println("Uma ou mais chaves não existe.\n");}
                                }
                            }
                            case 3 -> {
                                // Add server
                                co.addDataServer();
                            }
                            case 4 ->{
                                co.logout();
                                logged=false;
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
