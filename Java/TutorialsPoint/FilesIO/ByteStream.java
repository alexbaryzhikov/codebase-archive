import java.io.*;

public class ByteStream {
    public static void main(String[] args) throws IOException {
        int i, c;
        ByteArrayOutputStream bOutput = new ByteArrayOutputStream(12);
        ByteArrayInputStream bInput = null;

        // get the inputs from the user
        while(bOutput.size() != 10)
            bOutput.write("hello".getBytes());
        byte[] b = bOutput.toByteArray();

        // print the content of output stream
        for(i = 0 ; i < b.length; i++)
            System.out.print((char)b[i]);
        System.out.println();

        // convert characters to upper case
        bInput = new ByteArrayInputStream(b);
        for(i = 0; i < 1; i++) {
            while((c = bInput.read()) != -1)
                System.out.print(Character.toUpperCase((char)c));
            System.out.println();
            bInput.reset(); 
        }
    }
}
