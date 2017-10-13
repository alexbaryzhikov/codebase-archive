/* Test the performance of ArrayList vs LinkedList */
import java.util.*;

public class ArrayVsListTest {
    public static void main(String[] args) {
        int i;
        long startTime, endTime, duration;
        ArrayList<Integer> arrayList = new ArrayList<>();
        LinkedList<Integer> linkedList = new LinkedList<>();

        // ArrayList add
        startTime = System.nanoTime();
        for (i = 0; i < 100000; i++)
            arrayList.add(i);
        endTime = System.nanoTime();
        duration = endTime - startTime;
        System.out.printf("%20s %d\n", "ArrayList add:", duration);

        // LinkedList add
        startTime = System.nanoTime();
        for (i = 0; i < 100000; i++)
            linkedList.add(i);
        endTime = System.nanoTime();
        duration = endTime - startTime;
        System.out.printf("%20s %d\n", "LinkedList add:", duration);

        // ArrayList get
        startTime = System.nanoTime();
        for (i = 0; i < 10000; i++)
            arrayList.get(i);
        endTime = System.nanoTime();
        duration = endTime - startTime;
        System.out.printf("%20s %d\n", "ArrayList get:", duration);

        // LinkedList get
        startTime = System.nanoTime();
        for (i = 0; i < 10000; i++)
            linkedList.get(i);
        endTime = System.nanoTime();
        duration = endTime - startTime;
        System.out.printf("%20s %d\n", "LinkedList get:", duration);

        // ArrayList remove
        startTime = System.nanoTime();
        for (i = 9999; i >= 0; i--)
            arrayList.remove(i);
        endTime = System.nanoTime();
        duration = endTime - startTime;
        System.out.printf("%20s %d\n", "ArrayList remove:", duration);

        // LinkedList remove
        startTime = System.nanoTime();
        for (i = 9999; i >= 0; i--)
            linkedList.remove(i);
        endTime = System.nanoTime();
        duration = endTime - startTime;
        System.out.printf("%20s %d\n", "LinkedList remove:", duration);
    }
}
