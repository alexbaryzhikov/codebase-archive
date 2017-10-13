import java.util.LinkedHashSet;
import java.util.Set;

public class LinkedHashSet_ {

    public static void main(String[] args) {
        /* create a hash set */
        Set<String> hs = new LinkedHashSet<>();

        /* add elements to the hash set */
        hs.add("B");
        hs.add("A");
        hs.add("D");
        hs.add("E");
        hs.add("C");
        hs.add("F");

        /* iterate over set elements, in the order of addition */
        System.out.println("Set elements:");
        for (String s : hs)
            System.out.print(s + " ");
        System.out.println();
    }
}
