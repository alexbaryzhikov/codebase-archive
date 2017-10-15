/* --------------------------------------------------------
 * Intercepting Filter
 * --------------------------------------------------------
 *
 * The intercepting filter design pattern is used when we want to do some pre-processing or
 * post-processing with request or response of the application. Filters are defined and applied on
 * the request before passing the request to actual target application. Filters can do the
 * authentication, authorization, logging or tracking of request and then pass the requests to
 * corresponding handlers. Following are the entities of this type of design pattern.
 *
 * > Filter - performs certain task prior or after execution of request by request handler.
 *
 * > Filter Chain - carries multiple filters and help to execute them in defined order on target.
 *
 * > Target - request handler.
 *
 * > Filter Manager - manages filters and Filter Chain.
 *
 * > Client - sends request to Target.
 */

public class Main {
  public static void main(String[] args) {
    FilterManager filterManager = new FilterManager(new FilterChain(new Target()));
    filterManager.setFilter(new AuthenticationFilter());
    filterManager.setFilter(new DebugFilter());

    Client client = new Client(filterManager);
    client.sendRequest("HOME_REQUEST");
  }
}
