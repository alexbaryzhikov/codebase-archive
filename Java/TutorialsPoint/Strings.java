public class Strings {
    public static void main(String[] args) {
        String greeting = "Hello world!";

        char[] helloArray = { 'h', 'e', 'l', 'l', 'o', '.' };
        String helloString = new String(helloArray);
        System.out.println(helloString);

        /* StringBuffer/StringBuilder is a mutable string */
        StringBuilder sb = new StringBuilder("test");
        sb.append(" String Buffer");
        System.out.println(sb); 

        /* String length */
        String palindrome = "Dot saw I was Tod";
        int len = palindrome.length();
        System.out.println( "String Length is : " + len );

        /* Concatenation */
        String string1 = "saw I was ";
        System.out.println("Dot " + string1 + "Tod");

        /* Formatted string */
        double floatVar = 3.141592;
        int intVar = 100;
        String stringVar = "hello";
        String fs;
        fs = String.format("The value of the float variable is " +
                           "%f, while the value of the integer " +
                           "variable is %d, and the string " +
                           "is %s", floatVar, intVar, stringVar);
        System.out.println(fs);
    }
}
