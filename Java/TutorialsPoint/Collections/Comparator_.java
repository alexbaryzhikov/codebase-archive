import java.util.ArrayList;
import java.util.Collections;
import java.util.Comparator;
import java.util.List;

class Dog implements Comparator<Dog>, Comparable<Dog> {
    private String name;
    private int age;

    Dog() {
    }

    Dog(String n, int a) {
        name = n;
        age = a;
    }

    public String getName() {
        return name;
    }

    public int getAge() {
        return age;
    }

    /* Overriding the compareTo method */
    public int compareTo(Dog d) {
        return (this.name).compareTo(d.getName());
    }

    /* Overriding the compare method to sort the age  */
    public int compare(Dog d1, Dog d2) {
        return d1.getAge() - d2.getAge();
    }
}

public class Comparator_ {

    public static void main(String[] args) {
        /* Takes a list o Dog objects */
        List<Dog> list = new ArrayList<>();

        list.add(new Dog("Shaggy", 3));
        list.add(new Dog("Lacy", 2));
        list.add(new Dog("Roger", 10));
        list.add(new Dog("Tommy", 4));
        list.add(new Dog("Tammy", 1));

        /* Sort the array list */
        Collections.sort(list);
        for (Dog a: list)
            System.out.print(a.getName() + ", ");
        System.out.println();

        /* Sorts the array list using comparator */
        Collections.sort(list, new Dog());
        for (Dog a: list)
            System.out.print(a.getName() + " : " + a.getAge() + ", ");
        System.out.println();

        /* Sorts the array list using lambda */
        Collections.sort(list, (Dog d1, Dog d2) -> d1.getAge() - d2.getAge());
        for (Dog a: list)
            System.out.print(a.getName() + " : " + a.getAge() + ", ");
        System.out.println();
    }
}
