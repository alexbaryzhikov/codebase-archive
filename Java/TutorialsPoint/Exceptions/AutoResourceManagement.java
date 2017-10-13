import java.io.*;

public class AutoResourceManagement {
    public static void main(String[] args) {
        char[] buf = new char[50];

        /* acquire resources */
        try (FileReader fr = new FileReader("input.txt")) {
            fr.read(buf);
            for(char c : buf)
                System.out.print(c);
        } catch(IOException e) {
            e.printStackTrace();
        }
        /* resources are freed automatically */
    }
}

class ManualResourceManagement {
    public static void run() {
        char[] buf = new char[50];
        FileReader fr = null;

        try {
            /* acquire resources */
            File file = new File("input.txt");
            fr = new FileReader(file);
            fr.read(buf);
            for(char c : buf)
                System.out.print(c);
        } catch(IOException e) {
            e.printStackTrace();
        } finally {
            /* free resources */
            try {
                fr.close();
            } catch(IOException ex) {
                ex.printStackTrace();
            }
        }
    }
}
