public class Dispatcher {

  private HomeView homeView;
  private StudentView studentView;

  public Dispatcher(HomeView homeView, StudentView studentView) {
    this.homeView = homeView;
    this.studentView = studentView;
  }

  public void dispatch(String request) {
    if (request.equalsIgnoreCase("home")) {
      homeView.show();
    } else if (request.equalsIgnoreCase("student")) {
      studentView.show();
    }
  }
}
