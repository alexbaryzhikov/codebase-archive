public class FrontController {

  private Dispatcher dispatcher;

  public FrontController(Dispatcher dispatcher) {
    this.dispatcher = dispatcher;
  }

  public void dispatchRequest(String request) {
    // Log request
    trackRequest(request);

    // Authenticate user
    if (isAuthenticUser()) {
      dispatcher.dispatch(request);
    }
  }

  private boolean isAuthenticUser() {
    System.out.println("User is authenticated successfully");
    return true;
  }

  private void trackRequest(String request) {
    System.out.println("Page requested: " + request);
  }
}
