
import java.util.Scanner;

public class InputMenu {
    private String text;
    private String option;
    private String op;


    public InputMenu(String text,String option){
        this.text =text;
        this.option=option;
        this.op = "";
    }

    public void execute() {
        showMenu();
        this.op = readOption();
    }


    private void showMenu() {
        System.out.println(text);
    }

    private String readOption() {
        String op;
        Scanner is = new Scanner(System.in);

        System.out.print(this.option + ": ");
        op = is.nextLine();
        return op;
    }
    public String getOption() {
        return this.op;
    }



}