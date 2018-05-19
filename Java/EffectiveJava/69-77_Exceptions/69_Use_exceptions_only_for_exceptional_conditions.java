// Horrible abuse of exceptions. Don't ever do this!
try {
  int i = 0;
  while(true) {
    range[i++].climb();
  }
} catch(ArrayIndexOutOfBoundsException e) {
}


for (Mountain m : range) {
  m.climb();
}


// NOTE Exceptions are, as their name implies, to be used only for exceptional conditions; they
// should never be used for ordinary control flow.


// NOTE A well-designed API must not force its clients to use exceptions for ordinary control flow.


for (Iterator<Foo> i = collection.iterator(); i.hasNext(); ) {
  Foo foo = i.next();
  ...
}


// Do not use this hideous code for iteration over a collection!
try {
  Iterator<Foo> i = collection.iterator();
  while(true) {
    Foo foo = i.next();
    ...
  }
} catch (NoSuchElementException e) {
}
