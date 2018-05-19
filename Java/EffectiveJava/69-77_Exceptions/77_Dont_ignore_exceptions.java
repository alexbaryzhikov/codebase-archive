// Empty catch block ignores exception - Highly suspect!
try {
  ...
} catch (SomeException e) {
}


// NOTE An empty catch block defeats the purpose of exceptions.

// NOTE At the very least, the catch block should contain a comment explaining why it is
// appropriate to ignore the exception.
