// Deserialization bomb - deserializing this stream takes forever
static byte[] bomb() {
  Set<Object> root = new HashSet<>();
  Set<Object> s1 = root;
  Set<Object> s2 = new HashSet<>();
  for (int i = 0; i < 100; i++) {
    Set<Object> t1 = new HashSet<>();
    Set<Object> t2 = new HashSet<>();
    t1.add("foo"); // Make t1 unequal to t2
    s1.add(t1); s1.add(t2);
    s2.add(t1); s2.add(t2);
    s1 = t1;
    s2 = t2;
  }
  return serialize(root); // Method omitted for brevity
}

// NOTE The best way to avoid serialization exploits is never to deserialize anything.

// NOTE There is no reason to use Java serialization in any new system you write.

// NOTE Never deserialize untrusted data.

// NOTE Prefer whitelisting to blacklisting.
