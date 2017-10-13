public class ExtendsTest {

    public static void main(String[] args) {
        int a = 20, b = 10;
        My_Calculation demo = new My_Calculation();
        demo.addition(a, b);
        demo.subtraction(a, b);
        demo.multiplication(a, b);
    }
}

class Calculation {
    int z;

    public void addition(int x, int y) {
        z = x + y;
        System.out.println("The sum of the given numbers: " + z);
    }

    public void subtraction(int x, int y) {
        z = x - y;
        System.out.println("The difference between the given numbers: " + z);
    }
}

class My_Calculation extends Calculation {

    // overriding
    public void subtraction(int x, int y) {
        double z = Math.sqrt(Math.pow(x, 2) - Math.pow(y, 2));
        System.out.println("The difference between the given numbers: " + z);
    }

    // extending
    public void multiplication(int x, int y) {
        z = x * y;
        System.out.println("The product of the given numbers: " + z);
    }
}
