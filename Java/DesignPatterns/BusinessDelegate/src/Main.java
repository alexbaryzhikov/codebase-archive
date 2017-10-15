/* --------------------------------------------------------
 * Business Delegate
 * --------------------------------------------------------
 *
 * Intent
 *
 * - Decouple presentation tier and business tier
 */

public class Main {
  public static void main(String[] args) {
    Client client = new Client(new BusinessDelegate());

    client.getDelegate().setServiceType("EJB");
    client.doTask();

    client.getDelegate().setServiceType("JMS");
    client.doTask();
  }
}
