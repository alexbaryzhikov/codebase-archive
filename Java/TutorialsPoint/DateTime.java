import java.util.Date;
import java.text.*;

public class DateTime {
    public static void main(String[] args) {
        // Instantiate a Date object
        Date date = new Date();

        // display time and date using toString()
        System.out.println(date.toString());

        // Date formatting using SimpleDateFormat
        SimpleDateFormat ft = new SimpleDateFormat("E yyyy.MM.dd 'at' hh:mm:ss a zzz");
        System.out.println("Current Date: " + ft.format(date));

        // Date formatting using printf
        String str = String.format("Current Date/Time : %tc\n", date);
        System.out.printf(str);
        System.out.printf("%1$s %2$tB %2$td, %2$tY\n", "Due date:", date);
        System.out.printf("%s %tB %<te, %<tY\n", "Due date:", date);

        // Parsing strings into dates
        SimpleDateFormat ft2 = new SimpleDateFormat("yyyy-MM-dd"); 
        String input = "1818-11-112"; 
        Date t;
        System.out.print(input);
        try {
            t = ft2.parse(input); 
            System.out.println(" parses as " + t); 
        } catch (ParseException e) { 
            System.out.println(" unparseable using " + ft2.toPattern()); 
        }

        // Sleeping for a while
        try { 
            System.out.println(new Date()); 
            Thread.sleep(3000);  // 3 sec
            System.out.println(new Date()); 
        } catch (Exception e) {
            System.out.println("Got an exception!"); 
        }

        // Measuring elapsed time
        try {
            long start = System.currentTimeMillis();
            System.out.println(new Date());
            Thread.sleep(3000);
            System.out.println(new Date());
            long end = System.currentTimeMillis();
            long diff = end - start;
            System.out.println("Time elapsed: " + diff/1000.0 + "s");
        } catch (Exception e) {
            System.out.println("Got an exception!");
        }

    }
}
