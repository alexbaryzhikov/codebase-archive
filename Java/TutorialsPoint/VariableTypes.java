public class VariableTypes {
    public static void main(String args[]) {
        long t = System.nanoTime();
        Employee empOne = new Employee("Ransika");
        empOne.setSalary(1000);
        empOne.printEmp();
        Puppy p = new Puppy();
        p.pupAge();
        System.out.printf("%fs\n", (System.nanoTime()-t)*1e-9);
    }
}

class Employee {

    // this instance variable is visible for any child class.
    public String name;

    // salary  variable is visible in Employee class only.
    private double salary;

    // DEPARTMENT is a constant
    public static final String DEPARTMENT = "Development ";

    // The name variable is assigned in the constructor.
    public Employee (String empName) {
        name = empName;
    }

    // The salary variable is assigned a value.
    public void setSalary(double empSal) {
        salary = empSal;
    }

    // This method prints the employee details.
    public void printEmp() {
        System.out.println("name       : " + name );
        System.out.println("salary     : " + salary);
        System.out.println("department : " + DEPARTMENT);
        System.out.println();
    }
}

class Puppy {
    public void pupAge() {
        int age = 0;  /* initialization of local variable is mandatory */
        age = age + 7;
        System.out.println("Puppy age is " + age);
        System.out.println();
    }
}
