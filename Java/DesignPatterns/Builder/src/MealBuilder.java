import items.ChickenBurger;
import items.Coke;
import items.Pepsi;
import items.VeganBurger;

/**
 * Builder contains methods to create different flavors of meals
 */
public class MealBuilder {

  public Meal prepareVeganMeal() {
    Meal meal = new Meal();
    meal.addItem(new VeganBurger());
    meal.addItem(new Coke());
    return meal;
  }

  public Meal prepareNonVeganMeal() {
    Meal meal = new Meal();
    meal.addItem(new ChickenBurger());
    meal.addItem(new Pepsi());
    return meal;
  }
}
