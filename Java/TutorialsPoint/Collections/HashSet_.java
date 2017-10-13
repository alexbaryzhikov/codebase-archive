import java.util.Arrays;
import java.util.HashSet;
import java.util.Set;

public class HashSet_ {

    public static void main(String[] args) {
        /* create a hash set */
        Set<String> hs1 = new HashSet<>();
        Set<String> hs2 = new HashSet<>();

        /* add elements to the hash set */
        hs1.addAll(Arrays.asList("B", "A", "D", "E"));
        hs2.addAll(Arrays.asList("V", "A", "F", "E", "S"));

        System.out.println("Initial sets:");
        System.out.println(hs1);
        System.out.println(hs2);

        /* Union */
        Set<String> hs3 = setOR(hs1, hs2);
        System.out.println("\nUnion:");
        System.out.println(hs3);

        /* Intersection */
        Set<String> hs4 = setAND(hs1, hs2);
        System.out.println("\nIntersection:");
        System.out.println(hs4);

        /* Difference */
        Set<String> hs5a = setDIF(hs1, hs2);
        Set<String> hs5b = setDIF(hs2, hs1);
        System.out.println("\nDifference:");
        System.out.println(hs5a + ", " + hs5b);

        /* Exclusive OR */
        Set<String> hs6 = setXOR(hs1, hs2);
        System.out.println("\nExclusive OR:");
        System.out.println(hs6);

    }

    static <E> Set<E> setOR(Set<E> A, Set<E> B) {
        Set<E> C = new HashSet<>(A);
        C.addAll(B);
        return C;
    }

    static <E> Set<E> setAND(Set<E> A, Set<E> B) {
        Set<E> C = new HashSet<>(A);
        C.retainAll(B);
        return C;
    }

    static <E> Set<E> setDIF(Set<E> A, Set<E> B) {
        Set<E> C = new HashSet<>(A);
        C.removeAll(B);
        return C;
    }

    static <E> Set<E> setXOR(Set<E> A, Set<E> B) {
        return setOR(setDIF(A, B), setDIF(B, A));
    }

}
