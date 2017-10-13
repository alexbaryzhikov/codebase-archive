import java.io.*;

public class TryCatch {
    public static void main(String[] args) {
        int[] a = new int[]{6, 33};

        /* try -- catch -- finally */
        try {
            System.out.println("Access element three :" + a[3]);
        } catch (ArrayIndexOutOfBoundsException e) {
            System.out.println("Exception: " + e);
        } finally {
            System.out.println("First element value: " + a[0]);
            System.out.println("The 'finally' statement is executed");
        }
        System.out.println("Out of the block");

        /* multiple catches */
        try {
            FileInputStream f = new FileInputStream("no_file");
            int x = f.read();
        } catch(FileNotFoundException e) {
            System.out.println("File not found");
        } catch(IOException e) {
            System.out.println("I/O Exception");
        }

        /* catching multiple exceptions */
        try {
            FileInputStream f = new FileInputStream("no_file");
            int x = f.read();
        } catch(RuntimeException | IOException e) {
            e.printStackTrace();
        }
    }
}
