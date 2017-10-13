public class InstanceofTest {

    public static void main(String[] args) {
        Mammal m = new Mammal();
        Dog d = new Dog();

        System.out.println(d instanceof Mammal);
        System.out.println(d instanceof Animal);
    }
}

interface Animal {
}

class Mammal implements Animal {
}

class Reptile implements Animal {
}

class Dog extends Mammal {
}
