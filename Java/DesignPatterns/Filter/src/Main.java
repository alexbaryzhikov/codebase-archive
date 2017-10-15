/* --------------------------------------------------------
 * Filter / Criteria
 * --------------------------------------------------------
 *
 * Intent
 *
 * - Enables developers to filter a set of objects using different criteria and chaining them
 *   in a decoupled way through logical operations
 */

import java.util.ArrayList;
import java.util.List;

public class Main {
  public static void main(String[] args) {
    List<Person> persons = new ArrayList<>();

    persons.add(new Person("Robert","Male", "Single"));
    persons.add(new Person("John", "Male", "Married"));
    persons.add(new Person("Laura", "Female", "Married"));
    persons.add(new Person("Diana", "Female", "Single"));
    persons.add(new Person("Mike", "Male", "Single"));
    persons.add(new Person("Bobby", "Male", "Single"));

    Criteria male = new CriteriaMale();
    Criteria female = new CriteriaFemale();
    Criteria single = new CriteriaSingle();
    Criteria singleMale = new AndCriteria(single, male);
    Criteria singleOrFemale = new OrCriteria(single, female);

    System.out.println("Males");
    printPersons(male.meetCriteria(persons));

    System.out.println();
    System.out.println("Females");
    printPersons(female.meetCriteria(persons));

    System.out.println();
    System.out.println("Single males");
    printPersons(singleMale.meetCriteria(persons));

    System.out.println();
    System.out.println("Singles or females");
    printPersons(singleOrFemale.meetCriteria(persons));
  }

  private static void printPersons(List<Person> persons) {
    for (Person person : persons) {
      System.out.println(String.format("%s, %s, %s", person.getName(), person.getGender(),
          person.getMaritalStatus()));
    }
  }
}
