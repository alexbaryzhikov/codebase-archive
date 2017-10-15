package items;

public class VeganBurger extends Burger {

  @Override
  public float price() {
    return 25.0f;
  }

  @Override
  public String name() {
    return "Vegan Burger";
  }
}
