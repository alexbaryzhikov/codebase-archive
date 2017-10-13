import java.util.regex.*;

public class Regex {
    
    /* Find sequence of digits in input string */
    public static void findNumber() {
        /* String to be scanned to find the pattern. */
        String line = "This order was placed for QT3000! OK?";
        String pattern = "([^\\d]*)(\\d+)([^\\d]*)";

        /* Create a Pattern object */
        Pattern r = Pattern.compile(pattern);

        /* Now create matcher object. */
        Matcher m = r.matcher(line);
        if (m.find()) {
            System.out.println("Full expression: " + m.group(0));
            System.out.println("Group 1: " + m.group(1));
            System.out.println("Group 2: " + m.group(2));
            System.out.println("Group 3: " + m.group(3));
        } else
            System.out.println("NO MATCH");
    }

    /* Count the number of times the word "cat" appears in the input string */
    public static void findCat() {
        final String REGEX = "\\bcat\\b";
        final String INPUT = "cat cat cat cattie cat";
        Pattern p = Pattern.compile(REGEX);
        Matcher m = p.matcher(INPUT);   /* get a matcher object */
        int count = 0;
        while (m.find()) {
            count++;
            System.out.println("Match number " + count);
            System.out.println("start(): " + m.start());
            System.out.println("end(): " + m.end());
        }
    }

    /* lookingAt() method vs matches() */
    public static void lookingAtDemo() {
        final String REGEX = "foo";
        final String INPUT = "fooooooooooooooooo";
        Pattern pattern = Pattern.compile(REGEX);
        Matcher matcher = pattern.matcher(INPUT);
        System.out.println("Current REGEX is: " + REGEX);
        System.out.println("Current INPUT is: " + INPUT);
        System.out.println("lookingAt(): " + matcher.lookingAt());  /* match from the beginning */
        System.out.println("matches(): " + matcher.matches());      /* match entire region */
    }

    /* Replace all occurences of pattern in input string */
    public static void replacement() {
        final String REGEX = "a*b";
        final String INPUT = "aabfooaabfooabfoob";
        final String REPLACE = "-";
        Pattern p = Pattern.compile(REGEX);
        Matcher m = p.matcher(INPUT);
        StringBuffer sb = new StringBuffer();
        while(m.find())
            m.appendReplacement(sb, REPLACE);
        m.appendTail(sb);
        System.out.println(sb.toString());
    }

    public static void main(String[] args) {
        findNumber();
        findCat();
        lookingAtDemo();
        replacement();
    }
}
