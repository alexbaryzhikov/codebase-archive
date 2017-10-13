import java.util.Arrays;
import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

public class Algorithms {
    /* Collections as Cl */
    private static Collections Cl;

    public static void main(String[] args) {

        /* Create and initialize list */
        List<Integer> ll = new ArrayList<>(Arrays.asList(-8, 20, -20, 8, 0, 99));

        /* Create a reverse order comparator */
        Comparator<Integer> r = Cl.reverseOrder();

        /* Sort list by using the comparator */
        Cl.sort(ll, r);
        System.out.print("List sorted in reverse: ");
        for (Integer i : ll)
            System.out.print(i + " ");
        System.out.println();

        /* Shuffle list */
        Cl.shuffle(ll);
        System.out.print("List shuffled: ");
        for (Integer i : ll)
            System.out.print(i + " ");
        System.out.println();

        System.out.println("Minimum: " + Cl.min(ll));
        System.out.println("Maximum: " + Cl.max(ll));
    }
}
