import java.util.IdentityHashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;

public class IdentityHashMap_ {

    public static void main(String[] args) {
        /* Create a hash map */
        Map<String,Double> ihm = new IdentityHashMap<>();

        /* Put elements to the map */
        ihm.put("Zara", new Double(3434.34));
        ihm.put("Mahnaz", new Double(123.22));
        ihm.put("Ayan", new Double(1378.00));
        ihm.put("Daisy", new Double(99.22));
        ihm.put("Qadir", new Double(-19.08));

        /* Get a set of the entries */
        Set set = ihm.entrySet();

        /* Get an iterator */
        Iterator i = set.iterator();

        /* Display elements */
        while(i.hasNext()) {
            Map.Entry me = (Map.Entry)i.next();
            System.out.printf("%-8s: %8.2f\n", me.getKey(), me.getValue());
        }
    }
}
