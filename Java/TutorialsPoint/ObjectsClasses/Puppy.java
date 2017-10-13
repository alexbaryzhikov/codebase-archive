public class Puppy {
    String name;
    int age;

    public Puppy(String name) {
        // This constructor has one parameter, name.
        System.out.println("Puppy is named " + name);
        this.name = name;
    }

    public void setAge(int age) {
        this.age = age;
    }

    public int getAge() {
        System.out.println(this.name + "'s age is " + this.age);
        return this.age;
    }

    public static void main(String[] args) {
        /* Object creation */
        Puppy myPuppy = new Puppy("Tommy");

        /* Call class method to set puppy's age */
        myPuppy.setAge(2);

        /* Call another class method to get puppy's age */
        myPuppy.getAge();

        /* You can access instance variable as follows as well */
        System.out.println("Variable Value: " + myPuppy.age);
    }
}
