public class StudentController {

  private StudentModel model;
  private StudentView view;

  public StudentController(StudentModel model, StudentView view) {
    this.model = model;
    this.view = view;
  }

  public void setStudentName(String name) {
    model.setName(name);
  }

  public String getStudentName() {
    return model.getName();
  }

  public void setStudentMajor(String major) {
    model.setMajor(major);
  }

  public String getStudentMajor() {
    return model.getMajor();
  }

  public void updateView() {
    view.printStudent(model.getName(), model.getMajor());
  }
}
