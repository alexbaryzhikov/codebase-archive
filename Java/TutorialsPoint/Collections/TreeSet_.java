import java.util.Set;
import java.util.TreeSet;

public class TreeSet_ {

    public static void main(String[] args) {
        /* create a hash set */
        Set<String> hs = new TreeSet<>();

        /* add elements to the hash set */
        hs.add("B");
        hs.add("A");
        hs.add("D");
        hs.add("E");
        hs.add("C");
        hs.add("F");

        /* iterate over set elements, sorted order */
        System.out.println("Set elements:");
        for (String s: hs)
            System.out.print(s + " ");
        System.out.println();
    }
}
