/* --------------------------------------------------------
 * Data Access Object (DAO)
 * --------------------------------------------------------
 *
 * Data Access Object Pattern or DAO pattern is used to separate low level data accessing API or
 * operations from high level business services. Following are the participants in Data Access
 * Object Pattern.
 *
 * > Data Access Object Interface - This interface defines the standard operations to be performed
 *   on a model object(s).
 *
 * > Data Access Object concrete class - This class implements above interface. This class is
 *   responsible to get data from a data source which can be database / xml or any other storage
 *   mechanism.
 *
 * > Model Object or Value Object - This object is simple POJO containing get/set methods to store
 *   data retrieved using DAO class.
 */

public class Main {
  public static void main(String[] args) {
    StudentDao studentDao = new StudentDao();
    studentDao.add(new Student(5, "Robert", "Computer Science"));
    studentDao.add(new Student(1, "John", "Communications"));
    studentDao.add(new Student(44, "Delilah", "Political Science"));

    // Print all students
    for (Student s : studentDao.getAll()) {
      System.out.println("ID: " + s.getId() + ", Name: " + s.getName() + ", Major: "
          + s.getMajor());
    }

    // Update a student
    Student studentUpdate = new Student(1, "Michael", "Biology");
    studentDao.update(studentUpdate);

    // Get the student
    Student student = studentDao.get(1);
    System.out.println("ID: " + student.getId() + ", Name: " + student.getName() + ", Major: "
        + student.getMajor());

    // Delete a student
    studentDao.delete(5);

    // Print all students
    for (Student s : studentDao.getAll()) {
      System.out.println("ID: " + s.getId() + ", Name: " + s.getName() + ", Major: "
          + s.getMajor());
    }
  }
}
