import java.io.*;

public class ReadConsole {
    public static void main(String[] args) throws IOException {
        char c;
        InputStreamReader cin = null;

        try {
            cin = new InputStreamReader(System.in);
            System.out.println("Enter characters, 'q' to quit.");
            do {
                c = (char)cin.read();
                System.out.print(c);
            } while(c != 'q');
        } finally {
            System.out.println();
            if (cin != null)
                cin.close();
        }
    }
}
