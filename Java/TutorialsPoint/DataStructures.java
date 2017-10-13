import java.util.BitSet;
import java.util.EmptyStackException;
import java.util.Enumeration;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Properties;
import java.util.Set;
import java.util.Stack;
import java.util.Vector;

public class DataStructures {

    static void enumerationTest() {
        Enumeration days;
        Vector<String> dayNames = new Vector<>();

        System.out.println("Enumeration test ------------------\n");

        dayNames.add("Sunday");
        dayNames.add("Monday");
        dayNames.add("Tuesday");
        dayNames.add("Wednesday");
        dayNames.add("Thursday");
        dayNames.add("Friday");
        dayNames.add("Saturday");
        days = dayNames.elements();

        while (days.hasMoreElements()) {
            System.out.println(days.nextElement()); 
        }
        System.out.println();
    }

    static void bitsetTest() {
        BitSet bits1 = new BitSet(16);
        BitSet bits2 = new BitSet(16);

        System.out.println("BitSet test -----------------------\n");

        // set some bits
        for (int i = 0; i < 16; i++) {
            if ((i % 2) == 0) bits1.set(i);
            if ((i % 5) != 0) bits2.set(i);
        }

        System.out.println("Initial pattern in bits1: ");
        System.out.println(bits1);
        System.out.println("\nInitial pattern in bits2: ");
        System.out.println(bits2);

        // AND bits
        bits2.and(bits1);
        System.out.println("\nbits2 AND bits1: ");
        System.out.println(bits2);

        // OR bits
        bits2.or(bits1);
        System.out.println("\nbits2 OR bits1: ");
        System.out.println(bits2);

        // XOR bits
        bits2.xor(bits1);
        System.out.println("\nbits2 XOR bits1: ");
        System.out.println(bits2);
        System.out.println();
    }

    static void vectorTest() {
        // initial size is 3, increment is 2
        Vector<Number> v = new Vector<>(3, 2);

        System.out.println("Vector test -----------------------\n");

        System.out.println("Initial size: " + v.size());
        System.out.println("Initial capacity: " + v.capacity());

        v.addElement(new Integer(1));
        v.addElement(new Integer(2));
        v.addElement(new Integer(3));
        v.addElement(new Integer(4));
        System.out.println("Capacity after four additions: " + v.capacity());

        v.addElement(new Double(5.45));
        System.out.println("Current capacity: " + v.capacity());

        v.addElement(new Double(6.08));
        v.addElement(new Integer(7));
        System.out.println("Current capacity: " + v.capacity());

        v.addElement(new Float(9.4));
        v.addElement(new Integer(10));
        System.out.println("Current capacity: " + v.capacity());

        v.addElement(new Integer(11));
        v.addElement(new Integer(12));
        System.out.println("First element: " + (Integer)v.firstElement());
        System.out.println("Last element: " + (Integer)v.lastElement());

        if (v.contains(new Integer(3)))
            System.out.println("Vector contains 3");

        // enumerate the elements in the vector.
        Enumeration vEnum = v.elements();
        System.out.println("\nElements in vector:");

        while (vEnum.hasMoreElements())
            System.out.print(vEnum.nextElement() + " ");
        System.out.println('\n');
    }

    static void showpush(Stack<Integer> st, int a) {
        st.push(new Integer(a));
        System.out.println("push(" + a + ")");
        System.out.println("stack: " + st);
    }

    static void showpop(Stack<Integer> st) {
        System.out.print("pop -> ");
        Integer a = (Integer) st.pop();
        System.out.println(a);
        System.out.println("stack: " + st);
    }

    static void stackTest() {
        Stack<Integer> st = new Stack<>();

        System.out.println("Stack test ------------------------\n");

        System.out.println("stack: " + st);
        showpush(st, 42);
        showpush(st, 66);
        showpush(st, 99);
        showpop(st);
        showpop(st);
        showpop(st);
        try {
            showpop(st);
        } catch (EmptyStackException e) {
            System.out.println("empty stack");
        }
        System.out.println();
    }

    static void hashtableTest() {
        Hashtable<String, Double> balance = new Hashtable<>();
        Enumeration names;
        String str;
        double bal;

        System.out.println("Hashtable test --------------------\n");

        balance.put("Zara", new Double(3434.34));
        balance.put("Mahnaz", new Double(123.22));
        balance.put("Ayan", new Double(1378.00));
        balance.put("Daisy", new Double(99.22));
        balance.put("Qadir", new Double(-19.08));

        // Show all balances in hash table.
        names = balance.keys();
        while (names.hasMoreElements()) {
            str = (String) names.nextElement();
            System.out.printf("%-8s: %5.2f\n", str, balance.get(str));
        }        
        System.out.println();

        // Deposit 1,000 into Zara's account
        balance.put("Zara", new Double(balance.get("Zara") + 1000.00));
        System.out.println("Zara's new balance: " + balance.get("Zara"));
        System.out.println();
    }

    static void propertiesTest() {
        Properties capitals = new Properties();
        Set states;
        String str;

        System.out.println("Properties test -------------------\n");

        capitals.put("Illinois", "Springfield");
        capitals.put("Missouri", "Jefferson City");
        capitals.put("Washington", "Olympia");
        capitals.put("California", "Sacramento");
        capitals.put("Indiana", "Indianapolis");

        // Show all states and capitals in hashtable.
        states = capitals.keySet();  // get set-view of keys
        Iterator itr = states.iterator();

        while (itr.hasNext()) {
            str = (String) itr.next();
            System.out.println("The capital of " + str + " is " + capitals.getProperty(str));
        }     
        System.out.println();

        // look for state not in list -- specify default
        str = capitals.getProperty("Florida", "Not Found");
        System.out.println("The capital of Florida is " + str);
        System.out.println();
    }

    public static void main(String[] args) {
        enumerationTest();
        bitsetTest();
        vectorTest();
        stackTest();
        hashtableTest();
        propertiesTest();
    }
}
