import java.io.*;

public class DataStream {
    public static void main(String[] args) throws IOException {
        String s;
        DataOutputStream dataOut;
        DataInputStream dataIn;

        // write string to a file encoded as modified UTF-8
        dataOut = new DataOutputStream(new FileOutputStream("output.txt"));
        dataOut.writeUTF("hello\n");
        dataOut.close();

        // read data from the same file
        dataIn = new DataInputStream(new FileInputStream("output.txt"));
        while (dataIn.available() > 0) {
            s = dataIn.readUTF();
            System.out.print(s.toUpperCase());
        }
    }
}
