/* --------------------------------------------------------
 * Model-View-Controller (MVC)
 * --------------------------------------------------------
 *
 * Intent
 *
 * - Separate application's concerns
 *
 * > Model - represents an object carrying data. It can also have logic to update controller if
 *   its data changes
 * > View - represents the visualization of the data that model contains
 * > Controller - acts on both model and view. It controls the data flow into model object and
 *   updates the view whenever data changes. It keeps view and model separate
 */

public class Main {
  public static void main(String[] args) {
    StudentModel model = getStudentFromDatabase();
    StudentView view = new StudentView();
    StudentController controller = new StudentController(model, view);

    controller.updateView();

    // Change model data
    controller.setStudentName("John");
    controller.setStudentMajor("Communications");
    controller.updateView();
  }

  private static StudentModel getStudentFromDatabase() {
    StudentModel model = new StudentModel();
    model.setName("Robert");
    model.setMajor("Computer Science");
    return model;
  }
}
