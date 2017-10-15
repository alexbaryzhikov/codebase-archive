/* --------------------------------------------------------
 * Composite
 * --------------------------------------------------------
 *
 * Intent
 *
 * - Compose objects into tree structures to represent part-whole hierarchies
 * - Lets clients treat individual objects and compositions of objects uniformly
 */

import java.util.ArrayDeque;
import java.util.Deque;

public class Main {
  public static void main(String[] args) {
    Employee ceo = new Employee("John", "CEO", 30000);

    Employee headSales = new Employee("Robert", "Head Sales", 20000);

    Employee headMarketing = new Employee("Michel", "Head Marketing", 20000);

    Employee clerk1 = new Employee("Laura", "Marketing", 10000);
    Employee clerk2 = new Employee("Bob", "Marketing", 10000);

    Employee sales1 = new Employee("Richard", "Sales", 10000);
    Employee sales2 = new Employee("Rob", "Sales", 10000);

    ceo.add(headSales);
    ceo.add(headMarketing);

    headSales.add(sales1);
    headSales.add(sales2);

    headMarketing.add(clerk1);
    headMarketing.add(clerk2);

    mapHierarchy(ceo, System.out::println);
  }

  /**
   * Traverse hierarchy in breadth-first order, apply consumer to each element
   */
  private static void mapHierarchy(Employee root, EmployeeConsumer consumer) {
    Deque<Employee> deque = new ArrayDeque<>();
    Employee currentEmployee;
    // Add root to queue
    deque.add(root);
    while (!deque.isEmpty()) {
      // Get next item
      currentEmployee = deque.pop();
      consumer.apply(currentEmployee);
      // Add all children to queue
      for (Employee employee : currentEmployee.getSubordinates()) {
        deque.addLast(employee);
      }
    }
  }

  private interface EmployeeConsumer {
    void apply(Employee employee);
  }
}
