public class BoundedType {

    public static <T extends Comparable<T>> T maximum(T x, T y, T z) {
        /* Determine the largest of three Comparable objects. */
        T max = x;      /* assume x is initially the largest */
        if (y.compareTo(max) > 0)
            max = y;    /* y is the largest so far */
        if (z.compareTo(max) > 0)
            max = z;    /* z is the largest now */
        return max;     /* returns the largest object */
    }

    public static void main(String[] args) {
        System.out.printf("Max of (%d, %d, %d) is %d\n", 
            3, 4, 5, maximum(3, 4, 5));

        System.out.printf("Max of (%.1f, %.1f, %.1f) is %.1f\n",
            6.6, 8.8, 7.7, maximum(6.6, 8.8, 7.7));

        System.out.printf("Max of (%s, %s, %s) is %s\n",
            "pear", "apple", "orange", maximum("pear", "apple", "orange"));
    }
}
