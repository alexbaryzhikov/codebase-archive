/* --------------------------------------------------------
 * Command
 * --------------------------------------------------------
 *
 * Intent
 *
 * - Encapsulate a request in an object
 * - Allows the parameterization of clients with different requests
 * - Allows saving the requests in a queue
 *
 * Applicability
 *
 * - Parametrizes objects depending on the action they must perform
 * - Specifies or adds in a queue and executes requests at different moments in time
 * - Offers support for undoable actions (the execute() method can memorize the state and allow
 *   going back to that state)
 * - Structures the system in high level operations that based on primitive operations
 * - Decouples the object that invokes the action from the object that performs the action. Due
 *   to this usage it is also known as Producer - Consumer design pattern
 */

public class Main {
  public static void main(String[] args) {
    Stock btcStock = new Stock();

    BuyStock buyStockOrder = new BuyStock(btcStock);
    SellStock sellStockOrder = new SellStock(btcStock);

    Broker broker = new Broker();
    broker.takeOrder(buyStockOrder);
    broker.takeOrder(sellStockOrder);

    broker.placeOrders();
  }
}
