import java.util.Map;
import java.util.WeakHashMap;

public class WeakHashMap_ {
    private static Map<String,String> map;

    public static void main (String[] args) {
        map = new WeakHashMap<>();
        map.put(new String("Maine"), "Augusta");

        Runnable runner = new Runnable() {
            public void run() {
                String s = "|/" + (char)8213 + "\\";
                while (map.containsKey("Maine")) {
                    System.out.print("Thread waiting  ");
                    try {
                        for (int i = 0; i < 20; i++) {
                            System.out.print("\b" + s.charAt(i%4));
                            Thread.sleep(100);
                        }
                        System.out.println();
                    } catch (InterruptedException ignored) {
                    }
                    System.gc();
                }
            }
        };
        Thread t = new Thread(runner);
        t.start();
        System.out.println("Main waiting");
        try {
            t.join();
        } catch (InterruptedException ignored) {
        }
        System.out.println("Thread joined");
    }
}
