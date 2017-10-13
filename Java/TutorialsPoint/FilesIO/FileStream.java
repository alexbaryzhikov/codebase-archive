import java.io.*;

public class FileStream {
    public static void main(String[] args) {
        int i;
        byte[] bWrite = {11, 21, 3, 40, 5, 10};
        OutputStream os;
        InputStream is;

        try {
            os = new FileOutputStream("output.txt");
            for(byte b: bWrite)
                os.write(b);
            os.close();
            is = new FileInputStream("output.txt");
            while (is.available() > 0)
                System.out.print(is.read() + " ");
            System.out.println();
            is.close();
        } catch (IOException e) {
            System.out.print("Exception");
        }
    }
}
