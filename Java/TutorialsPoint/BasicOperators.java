public class BasicOperators {
    public static void main(String[] args) {
        int a, b;
        a = 10;
        String name = "James";
        boolean result;
        Vehicle bmw;

        /* Ternary */
        b = (a == 1) ? 20 : 30;
        System.out.println("Value of b is : " +  b);
        b = (a == 10) ? 20: 30;
        System.out.println("Value of b is : " + b);

        /* instaceof */
        result = name instanceof String;
        System.out.println(result);
        bmw = new Car();
        result =  bmw instanceof Car;
        System.out.println(result);
    }
}

class Vehicle {
}

class Car extends Vehicle {
}
