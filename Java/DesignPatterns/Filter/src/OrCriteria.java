import java.util.List;

public class OrCriteria implements Criteria {

  private Criteria criteriaA;
  private Criteria criteriaB;

  public OrCriteria(Criteria criteriaA, Criteria criteriaB) {
    this.criteriaA = criteriaA;
    this.criteriaB = criteriaB;
  }

  @Override
  public List<Person> meetCriteria(List<Person> persons) {
    List<Person> filteredListA = criteriaA.meetCriteria(persons);
    List<Person> filteredListB = criteriaB.meetCriteria(persons);
    for (Person person : filteredListB) {
      if (!filteredListA.contains(person)) {
        filteredListA.add(person);
      }
    }
    return filteredListA;
  }
}
