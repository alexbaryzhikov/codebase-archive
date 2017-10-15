import items.Item;

import java.util.ArrayList;
import java.util.List;

/**
 * Contains a list of food items
 */
public class Meal {

  private List<Item> items = new ArrayList<>();

  public void addItem(Item item) {
    items.add(item);
  }

  public float getCost() {
    float cost = 0.0f;
    for (Item item : items) {
      cost += item.price();
    }
    return cost;
  }

  public String toString() {
    StringBuilder s = new StringBuilder();
    for (Item item : items) {
      s.append("Item: ").append(item.name()).append(", packing: ").append(item.packing().pack())
          .append(", price: ").append(item.price()).append("\n");
    }
    return s.toString();
  }
}
