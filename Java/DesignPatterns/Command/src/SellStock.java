public class SellStock implements Order {

  private Stock btcStock;

  public SellStock(Stock btcStock) {
    this.btcStock = btcStock;
  }

  @Override
  public void execute() {
    btcStock.sell();
  }
}
