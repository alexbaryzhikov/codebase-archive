/* --------------------------------------------------------
 * Null Object
 * --------------------------------------------------------
 *
 * Intent
 *
 * - Instead of putting 'if' check for a null value, Null Object reflects a do nothing relationship
 */

public class Main {
  public static void main(String[] args) {
    System.out.println(CustomerFactory.create("Rob").getName());
    System.out.println(CustomerFactory.create("Bob").getName());
    System.out.println(CustomerFactory.create("Julie").getName());
    System.out.println(CustomerFactory.create("Laura").getName());
  }
}
