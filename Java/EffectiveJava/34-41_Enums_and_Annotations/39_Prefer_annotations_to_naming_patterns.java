// Marker annotation type declaration
import java.lang.annotation.*;

/**
 * Indicates that the annotated method is a test method.
 * Use only on parameterless static methods.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface Test {
}


// Program containing marker annotations
public class Sample {
  @Test public static void m1() { } // Test should pass

  public static void m2() { }

  @Test public static void m3() {   // Test should fail
    throw new RuntimeException("Boom");
  }

  public static void m4() { }

  @Test public void m5() { } // INVALID USE: nonstatic method

  public static void m6() { }

  @Test public static void m7() {   // Test should fail
    throw new RuntimeException("Crash");
  }

  public static void m8() { }
}


// Program to process marker annotations
import java.lang.reflect.*;

public class RunTests {
  public static void main(String[] args) throws Exception {
    int tests = 0;
    int passed = 0;
    Class testClass = Class.forName(args[0]);
    for (Method m : testClass.getDeclaredMethods()) {
      if (m.isAnnotationPresent(Test.class)) {
        tests++;
        try {
          m.invoke(null);
          passed++;
        } catch (InvocationTargetException wrappedExc) {
          Throwable exc = wrappedExc.getCause();
          System.out.println(m + " failed: " + exc);
        } catch (Exception exc) {
          System.out.println("INVALID @Test: " + m);
        }
      }
    }
    System.out.printf("Passed: %d, Failed: %d%n", passed, tests - passed);
  }
}

// -------------------------------------------------------------------------------------------------

// Annotation type with a parameter
import java.lang.annotation.*;

/**
 * Indicates that the annotated method is a test method that
 * must throw the designated exception to succeed.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface ExceptionTest {
  Class<? extends Exception> value();
}


// Program containing annotations with a parameter
public class Sample2 {

  @ExceptionTest(ArithmeticException.class)
  public static void m1() { // Test should pass
    int i = 0;
    i = i / i;
  }

  @ExceptionTest(ArithmeticException.class)
  public static void m2() { // Should fail (wrong exception)
    int[] a = new int[0];
    int i = a[1];
  }

  @ExceptionTest(ArithmeticException.class)
  public static void m3() { } // Should fail (no exception)
}


// Now let’s modify the test runner tool to process the new annotation.
if (m.isAnnotationPresent(ExceptionTest.class)) {
  tests++;
  try {
    m.invoke(null);
    System.out.printf("Test %s failed: no exception%n", m);
  } catch (InvocationTargetException wrappedEx) {
    Throwable exc = wrappedEx.getCause();
    Class<? extends Exception> excType = m.getAnnotation(ExceptionTest.class).value();
    if (excType.isInstance(exc)) {
      passed++;
    } else {
      System.out.printf("Test %s failed: expected %s, got %s%n", m, excType.getName(), exc);
    }
  } catch (Exception exc) {
    System.out.println("INVALID @Test: " + m);
  }
}

// -------------------------------------------------------------------------------------------------

// Annotation type with an array parameter
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface ExceptionTest {
  Class<? extends Exception>[] value();
}


// Code containing an annotation with an array parameter
@ExceptionTest({ IndexOutOfBoundsException.class, NullPointerException.class })
public static void doublyBad() {
  List<String> list = new ArrayList<>();

  // The spec permits this method to throw either
  // IndexOutOfBoundsException or NullPointerException
  list.addAll(5, null);
}


// Modify the test runner tool to process the new version of ExceptionTest
if (m.isAnnotationPresent(ExceptionTest.class)) {
  tests++;
  try {
    m.invoke(null);
    System.out.printf("Test %s failed: no exception%n", m);
  } catch (Throwable wrappedExc) {
    Throwable exc = wrappedExc.getCause();
    Class<? extends Exception>[] excTypes = m.getAnnotation(ExceptionTest.class).value();
    int oldPassed = passed;
    for (Class<? extends Exception> excType : excTypes) {
      if (excType.isInstance(exc)) {
        passed++;
        break;
      }
    }
    if (passed == oldPassed) {
      System.out.printf("Test %s failed: %s %n", m, exc);
    }
  }
}


// NOTE There is simply no reason to use naming patterns now that we have annotations.

// NOTE All programmers should, however, use the predefined annotation types provided by the Java
// platform
