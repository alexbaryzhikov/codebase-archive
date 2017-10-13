public class Methods {

    /* minimum between two numbers */
    public static int minFunction(int n1, int n2) {
        int min;
        if (n1 > n2)
            min = n2;
        else
            min = n1;
        return min; 
    }

    /* method overloading */
    public static double minFunction(double n1, double n2) {
        double min;
        if (n1 > n2)
            min = n2;
        else
            min = n1;
        return min; 
    }

    /* the void */
    public static void methodRankPoints(double points) {
        if (points >= 202.5)
            System.out.println("Rank: A1");
        else if (points >= 122.4)
            System.out.println("Rank: A2");
        else
            System.out.println("Rank: A3");
    }

    /* passing parameters by value */
    public static void swapFunction(int a, int b) {
        System.out.println("Before swapping (Inside), a = " + a + " b = " + b);
        int c = a;
        a = b;
        b = c;
        System.out.println("After swapping (Inside), a = " + a + " b = " + b);
    }

    /* finalize() method is called before instance destruction */
    protected void finalize() {
       // finalization code here
    }

    public static void main(String[] args) {
        int a = 11, b = 6;
        double c = 7.3, d = 9.4;
        int result1 = minFunction(a, b);
        double result2 = minFunction(c, d);  // same function name with different parameters
        System.out.println("Minimum Value = " + result1);
        System.out.println("Minimum Value = " + result2);

        methodRankPoints(255.7);

        System.out.println("Before swapping, a = " + a + " and b = " + b);
        swapFunction(a, b);
        System.out.println("After swapping, a = " + a + " and b is " + b); // a and b unchanged
    }
}
