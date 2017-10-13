import java.io.File;
import java.io.FileReader;

public class FileNotFound {
    public static void main(String[] args) {     
        File file = new File("file.txt");
        FileReader fr = new FileReader(file); 
    }
}
