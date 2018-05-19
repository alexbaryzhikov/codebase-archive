// NOTE Reflection allows one class to use another, even if the latter class did not exist when the
// former was compiled. This power, however, comes at a price:

// • You lose all the benefits of compile-time type checking, including exception checking. If a
//   program attempts to invoke a nonexistent or inaccessible method reflectively, it will fail at
//   runtime unless you’ve taken special precautions.

// • The code required to perform reflective access is clumsy and verbose. It is tedious to write
//   and difficult to read.

// • Performance suffers. Reflective method invocation is much slower than normal method
//   invocation. Exactly how much slower is hard to say, because there are so many factors at work.
//   On my machine, the speed difference can be as small as a factor of two or as large as a factor
//   of fifty.


// NOTE As a rule, objects should not be accessed reflectively in normal applications at runtime.

// NOTE You can obtain many of the benefits of reflection while incurring few of its costs by using
// it only in a very limited form.

// NOTE Create instances reflectively and access them normally via their interface or superclass.


// Reflective instantiation with interface access
public static void main(String[] args) {
  // Translate the class name into a Class object
  Class<?> cl = null;
  try {
    cl = Class.forName(args[0]);
  } catch(ClassNotFoundException e) {
    System.err.println("Class not found.");
    System.exit(1);
  }

  // Instantiate the class
  Set<String> s = null;
  try {
    s = (Set<String>) cl.newInstance();
  } catch(IllegalAccessException e) {
    System.err.println("Class not accessible.");
    System.exit(1);
  } catch(InstantiationException e) {
    System.err.println("Class not instantiable.");
    System.exit(1);
  }

  // Exercise the set
  s.addAll(Arrays.asList(args).subList(1, args.length));
  System.out.println(s);
}
