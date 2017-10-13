public class SuperTest {

    public static void main(String[] args) {
        Sub obj = new Sub(24);
        obj.getAge();
        obj.my_method();
    }
}

class Super {
    int age;
    int num = 20;

    Super(int age) {
        this.age = age;
    }

    public void getAge() {
        System.out.println("The value of the variable named age in superclass is: " + age);
    }

    // display method of superclass
    public void display() {
        System.out.println("This is the display method of superclass");
    }
}

class Sub extends Super {
    int num = 10;

    Sub(int age) {
        super(age);
    }

    // display method of sub class
    public void display() {
        System.out.println("This is the display method of subclass");
    }

    public void my_method() {
        // Invoking the display() method of subclass
        this.display();

        // Invoking the display() method of superclass
        super.display();

        // printing the value of variable num of subclass
        System.out.println("value of the variable named num in subclass: " + this.num);

        // printing the value of variable num of superclass
        System.out.println("value of the variable named num in superclass: " + super.num);
    }
}
