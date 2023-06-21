import org.junit.Assert;
import org.junit.Test;

import java.io.IOException;
import java.net.InetSocketAddress;
import java.util.*;
import java.util.concurrent.TimeUnit;


public class ClientTest {
    private String[] randomKeyNames = new String[]{
            "pao de forma","bolachas maria","brioche","broa",
            "croisant","baguete","bolo do caco","pao de água",
            "tostas","pretzels", "pao de lo", "pao integral",
            "papo seco", "pao alentejano"};
    private String[] randomValueNames = new String[]{
            "queijo","fiambre","manteiga","manteiga de amendoim",
            "geleia","nutella"
    };

    private InetSocketAddress addr = new InetSocketAddress("0.0.0.0",12345);
    private InetSocketAddress addr1 = new InetSocketAddress("0.0.0.0",12346);

    private boolean sameMap(Map<String,String> m1, Map<String,String> m2) {
        return m1.entrySet().equals(m2.entrySet());
    }


    @Test
    public void simpleTest() throws IOException {
        ClientOperations co1 = new ClientOperations(this.addr);
        co1.login("test");
        co1.writeValue("pao","manteiga");
        co1.readNValues(new String[]{"pao"});
        HashMap<String,String> expected = new HashMap<>();
        expected.put("pao","manteiga");
        co1.logout();
        var result = co1.returnResults();
        assert sameMap(expected,result);
    }

    @Test
    public void multipleReadsTest() throws IOException{
        // Fill the storage
        ClientOperations co1 = new ClientOperations(this.addr);
        co1.login("Ganso");
        co1.writeValue("pao","nutella");
        co1.writeValue("croissant","fiambre");
        co1.writeValue("baguete","queijo");
        co1.writeValue("broa","mel");


        // Diferent server
        ClientOperations co2 = new ClientOperations(this.addr1);
        co2.login("Luis");
        Map<String,String> read = co2.readNValues(new String[]{"pao","croissant","broa","baguete"});
        Map<String,String> expected = new HashMap<>();
        expected.put("pao","nutella");
        expected.put("croissant","fiambre");
        expected.put("baguete","queijo");
        expected.put("broa","mel");
        assert sameMap(expected,read);
    }



    @Test
    public void cachedValue() throws IOException{
        // Num primeiro servidor de sessão
        ClientOperations co1 = new ClientOperations( this.addr);
        co1.login("Ganso");
        co1.writeValue("pao","nutella");
        co1.logout();

        // Num segundo servidor de sessão
        ClientOperations co2 = new ClientOperations(this.addr1);
        co2.login("Ganso");
        //1st Read
        long start = System.nanoTime();
        co2.readNValues(new String[]{"pao"});
        long end = System.nanoTime();
        double elapsed =TimeUnit.NANOSECONDS.toNanos(end-start);
        System.out.printf("First read finished in: %.5f nanoseconds\n",elapsed);

        //2nd Read
        start = System.nanoTime();
        co2.readNValues(new String[]{"pao"});
        end = System.nanoTime();
        double elapsed2 = TimeUnit.NANOSECONDS.toNanos(end-start);
        System.out.printf("Second read finished in: %.5f nanoseconds\n",elapsed2);

        assert elapsed2 < elapsed;
    }


    @Test
    public void throttledTest() throws IOException, InterruptedException {
        int n = 0;
        ClientOperations co1 = new ClientOperations(this.addr);
        co1.login("Ganso");

        for(int i = 0; i < 60000;i++){
            co1.writeValue("pao","manteiga");
            System.out.println(i);
        }

        Thread.sleep(1000);
        co1.logout();
        Thread.sleep(1000);

        ClientOperations co2 = new ClientOperations(this.addr1);
        co2.login("Ganso");
        while (co2.writeValue("pao","manteiga")){
            n = n + 1;
        }

        co2.logout();
        assert n == 100;
    }

    @Test
    public void KeyPlacementChange() throws IOException{
        ClientOperations co1 = new ClientOperations(this.addr);
        ClientOperations co2 = new ClientOperations(this.addr1);
        co1.login("Ganso");
        co1.writeValue("pao","manteiga");
        for (int i=0;i<20;i++){
            co1.addDataServer();
        }
        co2.login("Bronze");
        co2.readNValues(new String[]{"pao"});
        HashMap<String,String> expected = new HashMap<>();
        expected.put("pao","manteiga");
        assert sameMap(expected,co2.returnResults());
    }


    @Test
    public void CausalCoherenceTest() throws IOException, InterruptedException {
        ClientOperations co1 = new ClientOperations(this.addr);
        ClientOperations co2 = new ClientOperations(this.addr1);
        co1.login("Ganso");
        co2.login("Luis");
        for (int i=0;i<1000;i++){
            int finalI = i;
            Thread writter = new Thread(()->{
                try {
                    co1.writeValue("pao","manteiga"+ finalI);
                    co1.writeValue("baguete","queijo"+ finalI);
                    co1.writeValue("broa","mel"+ finalI);
                } catch (IOException e) {
                    throw new RuntimeException(e);
                }
            });
            writter.start();
            co2.readNValues(new String[]{"pao","baguete","broa"});
            writter.join();
        }
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
            ClientOperations c = new ClientOperations(this.addr);
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
