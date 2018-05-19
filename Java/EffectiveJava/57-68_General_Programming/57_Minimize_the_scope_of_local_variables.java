// NOTE The most powerful technique for minimizing the scope of a local variable is to declare it
// where it is first used.

// NOTE Nearly every local variable declaration should contain an initializer.

// NOTE Prefer for loops to while loops.


// Preferred idiom for iterating over a collection
for (Element e : c) {
  doSomething(e);
}


// No for-each loop or generics before release 1.5
for (Iterator i = c.iterator(); i.hasNext(); ) {
  doSomething((Element) i.next());
}

// -------------------------------------------------------------------------------------------------

Iterator<Element> i = c.iterator();
while (i.hasNext()) {
  doSomething(i.next());
}
...
Iterator<Element> i2 = c2.iterator();
while (i.hasNext()) {  // BUG!
  doSomethingElse(i2.next());
}


for (Iterator<Element> i = c.iterator(); i.hasNext(); ) {
  doSomething(i.next());
}
...
// Compile-time error - cannot find symbol i
for (Iterator<Element> i2 = c2.iterator(); i.hasNext(); ) {
  doSomething(i2.next());
}

// -------------------------------------------------------------------------------------------------

for (int i = 0, n = expensiveComputation(); i < n; i++) {
  doSomething(i);
}


// NOTE Keep methods small and focused.
