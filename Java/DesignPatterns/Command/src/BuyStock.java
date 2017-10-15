public class BuyStock implements Order {

  private Stock btcStock;

  public BuyStock(Stock btcStock) {
    this.btcStock = btcStock;
  }

  @Override
  public void execute() {
    btcStock.buy();
  }
}
