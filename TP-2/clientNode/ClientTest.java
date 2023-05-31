import org.junit.Assert;
import org.junit.Test;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.Map;
import java.util.Random;
import java.util.concurrent.TimeUnit;


public class ClientTest {
    private String[] randomKeyNames = new String[]{
            "pão de forma","bolachas maria","brioche","broa",
            "croisant","baguete","bolo do caco","pão de água",
            "tostas","pretzels", "pão de ló", "pão integral",
            "papo seco", "pão alentejano"};
    private String[] randomValueNames = new String[]{
            "queijo","fiambre","manteiga","manteiga de amendoim",
            "geleia","nutella"
    };


    @Test
    public void simpleTest() throws IOException {
        ClientOperations co1 = new ClientOperations("Ganso", InetSocketAddress.createUnresolved("localhost",80));
        co1.writeValue("pão","manteiga");
        co1.readNValues(new String[]{"pão"});
        HashMap<String,String> expected = new HashMap<>();
        expected.put("pão","manteiga");
        Assert.assert
         co1.returnResults() == expected;
    }

    @Test
    public void multipleReadsTest() throws IOException{
        // Fill the storage
        ClientOperations co1 = new ClientOperations("Ganso", InetSocketAddress.createUnresolved("localhost",80));
        co1.writeValue("pão","nutella");
        co1.writeValue("croissant","fiambre");
        co1.writeValue("baguete","queijo");
        co1.writeValue("broa","mel");

        // Diferent server
        ClientOperations co2 = new ClientOperations("Ganso", InetSocketAddress.createUnresolved("localhost",80));
        Map<String,String> read = co2.readNValues(new String[]{"pão","croissant","broa","mel"});
        Map<String,String> expected = new HashMap<>();
        expected.put("pão","nutella");
        expected.put("croissant","fiambre");
        expected.put("baguete","queijo");
        expected.put("broa","mel");
        assert expected == read;

    }



    @Test
    public void cachedValue() throws IOException{
        // Num primeiro servidor de sessão
        ClientOperations co1 = new ClientOperations("Ganso", InetSocketAddress.createUnresolved("localhost",80));
        co1.writeValue("pão","nutella");

        // Num segundo servidor de sessão
        ClientOperations co2 = new ClientOperations("Ganso", InetSocketAddress.createUnresolved("localhost",80));
        //1st Read
        long start = System.nanoTime();
        co2.readNValues(new String[]{"pão"});
        long end = System.nanoTime();
        double elapsed =TimeUnit.NANOSECONDS.toMillis(end-start);
        System.out.printf("First read finished in: %.5f miliseconds",elapsed);

        //2nd Read
        start = System.nanoTime();
        co2.readNValues(new String[]{"pão"});
        end = System.nanoTime();
        double elapsed2 = TimeUnit.NANOSECONDS.toMillis(end-start);
        System.out.printf("Second read finished in: %.5f miliseconds",elapsed);

        assert elapsed2 < elapsed;
    }


    @Test
    public void throttledTest() throws IOException{
        ClientOperations co1 = new ClientOperations("Ganso", InetSocketAddress.createUnresolved("localhost",80));
        for (int i =0; i<200; i++){
            co1.writeValue("pão","manteiga");
        }
        // TODO: Check throttled
    }


    @Test
    public void CausalCoherenceTest() throws IOException{
        ClientOperations co1 = new ClientOperations("Ganso", InetSocketAddress.createUnresolved("localhost",80));
        ClientOperations co2 = new ClientOperations("Luis", InetSocketAddress.createUnresolved("localhost",80));
        co1.writeValue("pão","manteiga");
        new Thread(()->{
            try {
                co2.writeValue("pão","fiambre");
            } catch (IOException e) {
                throw new RuntimeException(e);
            }
        }).start();
        co1.readNValues(new String[]{"pão"});
        // Test expected
        HashMap<String,String> expected = new HashMap<>();
        expected.put("pão","manteiga");
        assert co1.returnResults() == expected;
    }

    // Maybe should check performance metrics
    private void profile1(ClientOperations co) {
        Random random = new Random();
        try {
            for (int i =0; i<10; i++){
                String key = this.randomKeyNames[random.nextInt(this.randomKeyNames.length+1)-1];
                String value = this.randomValueNames[random.nextInt(this.randomKeyNames.length+1)-1];
                co.writeValue(key,value);
            }
        }catch (Exception e) {
                throw new RuntimeException(e);
        }
    }
    private void profile2(ClientOperations co) {
        Random random = new Random();
        try {
        for (int i =0; i<10; i++){
            Thread.sleep(random.nextInt(100));
            String key = this.randomKeyNames[random.nextInt(this.randomKeyNames.length+1)-1];
            String value = this.randomValueNames[random.nextInt(this.randomKeyNames.length+1)-1];
            co.writeValue(key,value);
            co.readNValues(new String[]{key});
        }
        }catch (Exception e) {
            throw new RuntimeException(e);
        }
    }
    private void profile3(ClientOperations co) {
        Random random = new Random();
        String key = this.randomKeyNames[random.nextInt(this.randomKeyNames.length+1)-1];
        String value = this.randomValueNames[random.nextInt(this.randomKeyNames.length+1)-1];
        try {
            co.writeValue(key,value);
            co.readNValues(new String[]{key});
        } catch (IOException e) {
            throw new RuntimeException(e);
        }
    }


    @Test
    public void heavyLoadTest() throws IOException{
        int CLIENT_NUMBER = 1000;
        Random random = new Random();
        ArrayList<ClientOperations> clientOperations = new ArrayList<>();
        ArrayList<InetSocketAddress> sessions = new ArrayList<>();
        sessions.add(null);

        for (int i =0; i<CLIENT_NUMBER; i++){
            int session = random.nextInt(sessions.size())-1;
            int profile = random.nextInt(4);
            ClientOperations c = new ClientOperations("Client#"+i,sessions.get(session));
            switch (profile){
                case 1 ->{
                    new Thread(()->{
                        profile1(c);
                    }).start();
                    break;
                }
                case 2 ->{
                    new Thread(()->{
                        profile2(c);
                    }).start();
                    break;
                }
                case 3 ->{
                    new Thread(()->{
                        profile3(c);
                    }).start();
                    break;
                }
            }
        }
    }
}
