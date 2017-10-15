/* --------------------------------------------------------
 * Transfer Object
 * --------------------------------------------------------
 *
 * The Transfer Object pattern is used when we want to pass data with multiple attributes in one
 * shot from client to server. Transfer object is also known as Value Object. Transfer Object is
 * a simple POJO class having getter/setter methods and is serializable so that it can be
 * transferred over the network. It does not have any behavior. Server Side business class normally
 * fetches data from the database and fills the POJO and send it to the client or pass it by value.
 * For client, transfer object is read-only. Client can create its own transfer object and pass it
 * to server to update values in database in one shot.
 *
 * Following are the entities of this type of design pattern.
 *
 * > Business Object - Business Service fills the Transfer Object with data.
 *
 * > Value Object - Simple POJO having methods to set/get attributes only.
 *
 * > Client - Client either requests or sends the Transfer Object to Business Object.
 */

public class Main {
  public static void main(String[] args) {
    StudentBusinessObject studentBO = new StudentBusinessObject();

    // Create student objects
    ValueObject student1 = new ValueObject();
    student1.putInteger("ID", 32);
    student1.putString("NAME", "Enoch");
    student1.putString("MAJOR", "Sociology");
    studentBO.add(student1);
    ValueObject student2 = new ValueObject();
    student2.putInteger("ID", 55);
    student2.putString("NAME", "Lee");
    student2.putString("MAJOR", "Mathematics");
    studentBO.add(student2);
    ValueObject student3 = new ValueObject();
    student3.putInteger("ID", 200);
    student3.putString("NAME", "Stephanie");
    student3.putString("MAJOR", "Robotics");
    studentBO.add(student3);

    // Print all students
    for (ValueObject student : studentBO.getAll()) {
      System.out.print("ID: " + student.getInteger("ID"));
      System.out.print(", name: " + student.getString("NAME"));
      System.out.println(", major: " + student.getString("MAJOR"));
    }

    // Update student
    ValueObject student2Update = new ValueObject();
    student2Update.putInteger("ID", student2.getInteger("ID"));
    student2Update.putString("NAME", student2.getString("NAME"));
    student2Update.putString("MAJOR", "Arts and Design");
    studentBO.update(student2Update);

    // Get student
    ValueObject studentTmp = null;
    try {
      studentTmp = studentBO.get(55);
    } catch (StudentNotFoundException e) {
      System.err.println("StudentNotFoundException: " + e.getMessage());
    }
    if (studentTmp != null) {
      System.out.print("ID: " + studentTmp.getInteger("ID"));
      System.out.print(", name: " + studentTmp.getString("NAME"));
      System.out.println(", major: " + studentTmp.getString("MAJOR"));
    }

    // Delete student
    studentBO.delete(200);

    // Print all students
    for (ValueObject student : studentBO.getAll()) {
      System.out.print("ID: " + student.getInteger("ID"));
      System.out.print(", name: " + student.getString("NAME"));
      System.out.println(", major: " + student.getString("MAJOR"));
    }
  }
}
