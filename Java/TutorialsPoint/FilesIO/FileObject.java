import java.io.File;

public class FileObject {
    public static void main(String[] args) {
        boolean exe;
        String path;
        String[] fnames = {"testfile1.txt", "testfile2.txt"};
        File f = null;

        try {
            for(String s : fnames) {
                f = new File(s);
                exe = f.canExecute();
                path = f.getAbsolutePath();
                System.out.println(path + " is executable: " + exe);
            } 
        } catch (Exception e) {
            e.printStackTrace();
        }
    }
}
