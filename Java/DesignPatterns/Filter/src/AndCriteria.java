import java.util.List;

public class AndCriteria implements Criteria {

  private Criteria criteriaA;
  private Criteria criteriaB;

  public AndCriteria(Criteria criteriaA, Criteria criteriaB) {
    this.criteriaA = criteriaA;
    this.criteriaB = criteriaB;
  }

  @Override
  public List<Person> meetCriteria(List<Person> persons) {
    List<Person> filteredList = criteriaA.meetCriteria(persons);
    return criteriaB.meetCriteria(filteredList);
  }
}
