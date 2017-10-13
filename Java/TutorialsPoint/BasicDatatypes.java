public class BasicDatatypes {
    public static void main(String[] args) {
        byte    b = (byte)~(1<<7);
        short   s = (short)~(1<<15);
        int     i = ~(1<<31);
        long    l = ~(1L<<63);
        float   f = 3.14159265f;
        double  d = 2.718281828459045;
        boolean x = true;
        char    c = 'A';
        System.out.printf("byte    %d\n", b);
        System.out.printf("short   %d\n", s);
        System.out.printf("int     %d\n", i);
        System.out.printf("long    %d\n", l);
        System.out.printf("float   %f\n", f);
        System.out.printf("double  %f\n", d);
        System.out.printf("boolean %b\n", x);
        System.out.printf("char    %c\n", c);
    }
}
