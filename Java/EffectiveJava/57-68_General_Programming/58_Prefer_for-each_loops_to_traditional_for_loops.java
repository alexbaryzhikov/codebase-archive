// No longer the preferred idiom to iterate over a collection!
for (Iterator i = c.iterator(); i.hasNext(); ) {
  doSomething((Element) i.next()); // (No generics before 1.5)
}

// No longer the preferred idiom to iterate over an array!
for (int i = 0; i < a.length; i++) {
  doSomething(a[i]);
}


// The preferred idiom for iterating over collections and arrays
for (Element e : elements) {
  doSomething(e);
}

// -------------------------------------------------------------------------------------------------

// Can you spot the bug?
enum Suit { CLUB, DIAMOND, HEART, SPADE }
enum Rank { ACE, DEUCE, THREE, FOUR, FIVE, SIX, SEVEN, EIGHT, NINE, TEN, JACK, QUEEN, KING }
...
Collection<Suit> suits = Arrays.asList(Suit.values());
Collection<Rank> ranks = Arrays.asList(Rank.values());

List<Card> deck = new ArrayList<Card>();
for (Iterator<Suit> i = suits.iterator(); i.hasNext(); ) {
  for (Iterator<Rank> j = ranks.iterator(); j.hasNext(); ) {
    deck.add(new Card(i.next(), j.next()));
  }
}


// Same bug, different symptom!
enum Face { ONE, TWO, THREE, FOUR, FIVE, SIX }
...
Collection<Face> faces = Arrays.asList(Face.values());

for (Iterator<Face> i = faces.iterator(); i.hasNext(); ) {
  for (Iterator<Face> j = faces.iterator(); j.hasNext(); ) {
    System.out.println(i.next() + " " + j.next());
  }
}


// Fixed, but ugly - you can do better!
for (Iterator<Suit> i = suits.iterator(); i.hasNext(); ) {
  Suit suit = i.next();
  for (Iterator<Rank> j = ranks.iterator(); j.hasNext(); ) {
    deck.add(new Card(suit, j.next()));
  }
}


// Preferred idiom for nested iteration on collections and arrays
for (Suit suit : suits) {
  for (Rank rank : ranks) {
    deck.add(new Card(suit, rank));
  }
}

// -------------------------------------------------------------------------------------------------

public interface Iterable<E> {
  // Returns an iterator over the elements in this iterable
  Iterator<E> iterator();
}


// NOTE Three common situations where you can’t use a for-each loop:

// 1. Filtering — If you need to traverse a collection and remove selected elements, then you need
//    to use an explicit iterator so that you can call its remove method.

// 2. Transforming — If you need to traverse a list or array and replace some or all of the values
//    of its elements, then you need the list iterator or array index in order to set the value of
//    an element.

// 3. Parallel iteration — If you need to traverse multiple collections in parallel, then you need
//    explicit control over the iterator or index variable, so that all iterators or index
//    variables can be advanced in lockstep (as demonstrated unintentionally in the buggy card and
//    dice examples above).
