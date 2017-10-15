/* --------------------------------------------------------
 * Mediator
 * --------------------------------------------------------
 *
 * Intent
 *
 * - Encapsulates how a set of objects interact
 * - Promotes loose coupling by keeping objects from referring to each other explicitly
 *
 * Applicability
 *
 * - A set of objects communicate in well-defined but complex ways. The resulting interdependencies
 *   are unstructured and difficult to understand
 * - Reusing an object is difficult because it refers to and communicates with many other objects
 * - A behavior that's distributed between several classes should be customizable without a lot of
 *   subclassing
 *
 * Related Patterns
 *
 * - Facade Pattern - a simplified mediator becomes a facade pattern if the mediator is the only
 *   active class and the colleagues are passive classes. A facade pattern is just an implementation
 *   of the mediator pattern where mediator is the only object triggering and invoking actions on
 *   passive colleague classes. The Facade is being call by some external classes
 * - Adapter Pattern - the mediator patter just "mediate" the requests between the colleague
 *   classes. It is not supposed to change the messages it receives and sends; if it alters those
 *   messages then it is an Adapter pattern.
 * - Observer Pattern - the observer and mediator are similar patterns, the main difference between
 *   them is the problem they address. The observer pattern handles the communication between
 *   observers and subjects or subject. It's very probable to have new observable objects added.
 *   On the other hand in the mediator pattern the mediator class is most likely to be inherited
 */

public class Main {
  public static void main(String[] args) {
    User robert = new User("Robert");
    User john = new User("John");

    robert.sendMessage("Hi John!");
    john.sendMessage("Hello Robert!");
  }
}
