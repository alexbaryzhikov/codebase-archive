import java.util.Iterator;
import java.util.Map;
import java.util.Set;
import java.util.TreeMap;

public class TreeMap_ {

    public static void main(String[] args) {
        /* Create a hash map */
        Map<String,Double> tm = new TreeMap<>();

        /* Put elements to the map */
        tm.put("Zara", new Double(3434.34));
        tm.put("Mahnaz", new Double(123.22));
        tm.put("Ayan", new Double(1378.00));
        tm.put("Daisy", new Double(99.22));
        tm.put("Qadir", new Double(-19.08));

        /* Get a set of the entries */
        Set set = tm.entrySet();

        /* Get an iterator */
        Iterator i = set.iterator();

        /* Display elements  */
        Map.Entry me;
        while (i.hasNext()) {
            me = (Map.Entry) i.next();
            System.out.printf("%-8s: %8.2f\n", me.getKey(), me.getValue());
        }
    }
}
