public class Employee {
    String name;
    int age;
    String designation;
    double salary;

    // This is the constructor of the class Employee
    public Employee(String name) {
        this.name = name;
    }

    // Assign the age of the Employee  to the variable age.
    public void setAge(int age) {
        this.age = age;
    }

    /* Assign the designation to the variable designation.*/
    public void setDesignation(String desig) {
        this.designation = desig;
    }

    /* Assign the salary to the variable salary.*/
    public void setSalary(double salary) {
        this.salary = salary;
    }

    /* Print the Employee details */
    public void printEmployee() {
        System.out.println("Name:        " + name );
        System.out.println("Age:         " + age );
        System.out.println("Designation: " + designation );
        System.out.println("Salary:      " + salary);
        System.out.println();
    }
}
