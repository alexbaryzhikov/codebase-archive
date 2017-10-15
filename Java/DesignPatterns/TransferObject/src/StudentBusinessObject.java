import java.util.ArrayList;
import java.util.List;

public class StudentBusinessObject {

  // Database
  List<ValueObject> students = new ArrayList<>();

  public boolean add(ValueObject student) {
    return students.add(student);
  }

  public ValueObject get(int studentId) throws StudentNotFoundException {
    Integer id;
    for (ValueObject student : students) {
      id = student.getInteger("ID");
      if (id != null && id == studentId) {
        return student;
      }
    }
    throw new StudentNotFoundException("ID " + studentId);
  }

  public List<ValueObject> getAll() {
    return students;
  }

  public void delete(int studentId) {
    for (ValueObject student : students) {
      if (student.getInteger("ID") == studentId) {
        students.remove(student);
        System.out.println("Student " + studentId + " deleted");
        return;
      }
    }
  }

  public void update(ValueObject student) {
    ValueObject updatedStudent = null;
    try {
      Integer id = student.getInteger("ID");
      if (id != null) {
        updatedStudent = get(id);
      } else {
        System.err.println("Update error: argument has no ID");
      }
    } catch (StudentNotFoundException e) {
      System.err.println("StudentNotFoundException: " + e.getMessage());
      return;
    }
    if (updatedStudent != null) {
      students.remove(updatedStudent);
      students.add(student);
      System.out.println("Student " + student.getInteger("ID") + " updated");
    }
  }
}
