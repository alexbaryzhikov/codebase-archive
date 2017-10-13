import java.util.HashSet;
import java.util.Set;
import java.util.TreeSet;

public class Set_ {

    public static void main(String[] args) { 
        int[] count = { 34, 22, 10, 60, 30, 22 };
        Set<Integer> set = new HashSet<>();
        try {
            for (int i : count) set.add(i);
            System.out.println(set);

            TreeSet<Integer> sortedSet = new TreeSet<>(set);
            System.out.println("The sorted list is:");
            System.out.println(sortedSet);

            System.out.println("The first element of the set is: " + (Integer)sortedSet.first());
            System.out.println("The last element of the set is: " + (Integer)sortedSet.last());
        } catch (Exception e) {
            System.out.println("Oops! " + e);
        }
    }
}
