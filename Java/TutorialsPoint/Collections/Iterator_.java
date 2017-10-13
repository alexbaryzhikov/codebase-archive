import java.util.ArrayList;
import java.util.Arrays;
import java.util.Iterator;
import java.util.List;
import java.util.ListIterator;

public class Iterator_ {

    public static void main(String[] args) {
        /* Create an array list */
        List<String> al = new ArrayList<>();

        /* add elements to the array list */
        al.addAll(Arrays.asList("ABCDEF".split("")));

        /* Use iterator to display contents of al */
        System.out.print("Original contents of al: ");
        Iterator itr = al.iterator();
        while (itr.hasNext()) {
            Object element = itr.next();
            System.out.print(element + " ");
        }
        System.out.println();

        /* Modify objects being iterated */
        ListIterator<String> litr = al.listIterator();
        while (litr.hasNext()) {
            Object element = litr.next();
            litr.set(element + "+");
        }
        
        System.out.print("Modified contents of al: ");
        for (String s : al)
            System.out.print(s + " ");
        System.out.println();

        /* Now, display the list backwards */
        System.out.print("Modified list backwards: ");
        while (litr.hasPrevious()) {
            Object element = litr.previous();
            System.out.print(element + " ");
        }
        System.out.println();
    }
}
