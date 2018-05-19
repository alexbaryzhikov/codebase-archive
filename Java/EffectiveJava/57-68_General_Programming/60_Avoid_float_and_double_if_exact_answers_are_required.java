// NOTE The float and double types are particularly ill-suited for monetary calculations.

System.out.println(1.03 - .42);  // 0.6100000000000001

System.out.println(1.00 - 9 * .10);  // 0.09999999999999998


// Broken - uses floating point for monetary calculation!
public static void main(String[] args) {
  double funds = 1.00;
  int itemsBought = 0;
  for (double price = .10; funds >= price; price += .10) {
    funds -= price;
    itemsBought++;
  }
  System.out.println(itemsBought + " items bought.");
  System.out.println("Change: $" + funds);
}


// NOTE Use BigDecimal, int, or long for monetary calculations.


public static void main(String[] args) {
  final BigDecimal TEN_CENTS = new BigDecimal( ".10");
  int itemsBought = 0;
  BigDecimal funds = new BigDecimal("1.00");
  for (BigDecimal price = TEN_CENTS; funds.compareTo(price) >= 0; price = price.add(TEN_CENTS)) {
    itemsBought++;
    funds = funds.subtract(price);
  }
  System.out.println(itemsBought + " items bought.");
  System.out.println("Money left over: $" + funds);
}


public static void main(String[] args) {
  int itemsBought = 0;
  int funds = 100;
  for (int price = 10; funds >= price; price += 10) {
    itemsBought++;
    funds -= price;
  }
  System.out.println(itemsBought + " items bought.");
  System.out.println("Money left over: "+ funds + " cents");
}
