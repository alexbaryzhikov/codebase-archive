/* --------------------------------------------------------
 * Builder
 * --------------------------------------------------------
 *
 * Intent
 *
 * - Defines an instance for creating an object but letting subclasses decide which class
 *   to instantiate
 * - Refers to the newly created object through a common interface
 *
 * Applicability
 *
 * - Creation algorithm of a complex object is independent from the parts that actually compose
 *   the object
 * - System needs to allow different representations for the objects that are being built
 *
 * Examples
 *
 * - Vehicle manufacturer
 * - Students exams
 * - Interface layouts for guest, admins, and other users
 */

public class Main {
  public static void main(String[] args) {

    MealBuilder mealBuilder = new MealBuilder();

    // Use meal builder to create a vegan meal object
    Meal veganMeal = mealBuilder.prepareVeganMeal();
    System.out.println("Vegan Meal");
    System.out.print(veganMeal);
    System.out.println("Total cost: " + veganMeal.getCost());

    // Use meal builder to create a non-vegan meal object
    Meal nonVeganMeal = mealBuilder.prepareNonVeganMeal();
    System.out.println("\nNon-Vegan Meal");
    System.out.print(nonVeganMeal);
    System.out.println("Total cost: " + nonVeganMeal.getCost());
  }
}
