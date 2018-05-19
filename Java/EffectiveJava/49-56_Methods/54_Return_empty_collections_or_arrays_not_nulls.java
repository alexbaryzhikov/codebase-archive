private final List<Cheese> cheesesInStock = ...;

/**
 * @return an array containing all of the cheeses in the shop,
 *   or null if no cheeses are available for purchase.
 */
public Cheese[] getCheeses() {
  if (cheesesInStock.size() == 0) {
    return null;
  }
  ...
}


// Doing so requires extra code in the client to handle the null return value, for example:
Cheese[] cheeses = shop.getCheeses();
if (cheeses != null && Arrays.asList(cheeses).contains(Cheese.STILTON)) {
  System.out.println("Jolly good, just the thing.");
}
// instead of:
if (Arrays.asList(shop.getCheeses()).contains(Cheese.STILTON)) {
  System.out.println("Jolly good, just the thing.");
}


// The right way to return an array from a collection
private final List<Cheese> cheesesInStock = ...;
private static final Cheese[] EMPTY_CHEESE_ARRAY = new Cheese[0];

/**
 * @return an array containing all of the cheeses in the shop.
 */
public Cheese[] getCheeses() {
  return cheesesInStock.toArray(EMPTY_CHEESE_ARRAY);
}


// The right way to return a copy of a collection
public List<Cheese> getCheeseList() {
  if (cheesesInStock.isEmpty()) {
    return Collections.emptyList(); // Always returns same list
  } else {
    return new ArrayList<Cheese>(cheesesInStock);
  }
}


// NOTE There is no reason ever to return null from an array- or collection-valued method instead
// of returning an empty array or collection.
