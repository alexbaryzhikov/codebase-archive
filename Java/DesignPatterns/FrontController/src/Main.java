/* --------------------------------------------------------
 * Front Controller
 * --------------------------------------------------------
 *
 * The front controller design pattern is used to provide a centralized request handling mechanism
 * so that all requests will be handled by a single handler. This handler can do the authentication,
 * authorization, logging or tracking of requests and then pass them to corresponding handlers.
 * Following are the entities of this type of design pattern.
 *
 * > Front Controller - Single handler for all kinds of requests coming to the application (either
 *   web based or desktop based).
 *
 * > Dispatcher - Front Controller may use a dispatcher object which can dispatch the request to
 *   corresponding specific handler.
 *
 * > View - Views are the objects for which the requests are made.
 */

public class Main {
  public static void main(String[] args) {
    Dispatcher dispatcher = new Dispatcher(new HomeView(), new StudentView());
    FrontController frontController = new FrontController(dispatcher);
    frontController.dispatchRequest("HOME");
    frontController.dispatchRequest("STUDENT");
  }
}
