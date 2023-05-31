import java.util.Arrays;
import java.util.InputMismatchException;
import java.util.List;
import java.util.Scanner;

public class Menu {
    private List<String> options;
    private int op;
    private String  text;

    public Menu(String text, String[] opcoes){
        this.options =Arrays.asList(opcoes);
        this.text = text;
        this.op = 0;
    }

    public void executa() {
        do {
            showMenu();
            this.op = readOption();
        } while (this.op == -1);
    }


    private void showMenu() {
        System.out.println("\n *** " +this.text+ " *** ");
        for (int i=0; i<this.options.size(); i++) {
            System.out.print(i+1);
            System.out.print(" - ");
            System.out.println(this.options.get(i));
        }
        System.out.println("0 - Go back to login");
    }

    private int readOption() {
        int op;
        Scanner is = new Scanner(System.in);

        System.out.print("Option: ");
        try {
            op = is.nextInt();
        }
        catch (InputMismatchException e) {
            op = -1;
        }
        if (op<0 || op>this.options.size()) {
            System.out.println("Invalid Option!!!");
            op = -1;
        }
        return op;
    }
    public int getOption() {
        return this.op;
    }



}