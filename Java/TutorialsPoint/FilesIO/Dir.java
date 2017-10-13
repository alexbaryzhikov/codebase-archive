import java.io.File;
import java.util.Arrays;

public class Dir {
    public static void makeDir() {
        String dirname = "/tmp/user/java/bin";
        File d = new File(dirname);

        // Create directory
        if (d.mkdirs())
            System.out.println(dirname + " successfully created");
    }

    public static void listDir() {
        File d = null;
        String[] paths;

        try {      
            d = new File("/tmp");
            paths = d.list();
            Arrays.sort(paths);
            for(String path : paths)
                System.out.println(path);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    public static void main(String[] args) {
        // makeDir();
        listDir();
    }
}
