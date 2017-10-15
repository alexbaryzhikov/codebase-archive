/* --------------------------------------------------------
 * State
 * --------------------------------------------------------
 *
 * Intent
 *
 * - Create objects which represent various states and a context object whose behavior varies as
 *   its state object changes
 */

public class Main {
  public static void main(String[] args) {
    Context player = new Player();

    new StartState().apply(player);
    System.out.println("Player state - " + player.getState());
    new StopState().apply(player);
    System.out.println("Player state - " + player.getState());
  }
}
