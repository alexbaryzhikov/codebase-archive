import java.util.ArrayList;
import java.util.List;

public class StudentDao implements Dao<Student> {

  // Database
  private List<Student> students = new ArrayList<>();

  @Override
  public boolean add(Student student) {
    return students.add(student);
  }

  @Override
  public void update(Student student) {
    Student studentToUpdate = get(student.getId());
    studentToUpdate.setName(student.getName());
    studentToUpdate.setMajor(student.getMajor());
    System.out.println("Student " + studentToUpdate.getId() + " updated");
  }

  @Override
  public void delete(int studentId) {
    for (int i = 0; i < students.size(); i++) {
      if (students.get(i).getId() == studentId) {
        students.remove(i);
        System.out.println("Student " + studentId + " deleted");
        return;
      }
    }
    System.err.println("StudentDao::deleteStudent: Student not found: ID " + studentId);
  }

  @Override
  public Student get(int studentId) {
    for (Student student : students) {
      if (student.getId() == studentId) {
        return student;
      }
    }
    System.err.println("StudentDao::getStudent: Student not found: ID" + studentId);
    return null;
  }

  @Override
  public List<Student> getAll() {
    return students;
  }
}
