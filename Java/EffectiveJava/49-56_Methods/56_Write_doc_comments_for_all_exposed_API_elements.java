// NOTE To document your API properly, you must precede every exported class, interface,
// constructor, method, and field declaration with a doc comment.

// NOTE The doc comment for a method should describe succinctly the contract between the method
// and its client.


/**
 * Returns the element at the specified position in this list.
 * <p>
 * This method is <i>not</i> guaranteed to run in constant
 * time. In some implementations it may run in time proportional
 * to the element position.
 * <p>
 * @param index index of element to return; must be
 *        non-negative and less than the size of this list
 * @return the element at the specified position in this list
 * @throws IndexOutOfBoundsException if the index is out of range
 *         ({@code index < 0 || index >= this.size()})
 */
E get(int index);


// NOTE It is no longer necessary to use the HTML <code> or <tt> tags in doc comments: the Javadoc
// {@code} tag is preferable because it eliminates the need to escape HTML metacharacters.


// {@literal} tag, which suppress processing of HTML markup and nested Javadoc tags. It is like
// the {@code} tag, except that it doesn’t render the text in code font. For example, this Javadoc
// fragment:

/**
 * The triangle inequality is {@literal |x + y| < |x| + |y|}.
 */


// NOTE no two members or constructors in a class or interface should have the same summary
// description.


// Surround the offending period and any associated text with a {@literal} tag, so the period is
// no longer followed by a space in the source code:

/**
 * A college degree, such as B.S., {@literal M.S.} or Ph.D.
 * College is a fountain of knowledge where many go to drink.
 */
public class Degree { ... }


// NOTE When documenting a generic type or method, be sure to document all type parameters:

/**
 * An object that maps keys to values. A map cannot contain
 * duplicate keys; each key can map to at most one value.
 *
 * (Remainder omitted)
 *
 * @param <K> the type of keys maintained by this map
 * @param <V> the type of mapped values
 */
public interface Map<K, V> {
  ... // Remainder omitted
}


// NOTE When documenting an enum type, be sure to document the constants:

/**
 * An instrument section of a symphony orchestra.
 */
public enum OrchestraSection {
  /** Woodwinds, such as flute, clarinet, and oboe. */
  WOODWIND,

  /** Brass instruments, such as french horn and trumpet. */
  BRASS,

  /** Percussion instruments, such as timpani and cymbals */
  PERCUSSION,

  /** Stringed instruments, such as violin and cello. */
  STRING;
}


// NOTE When documenting an annotation type, be sure to document any members:

/**
 * Indicates that the annotated method is a test method that
 * must throw the designated exception to succeed.
 */
@Retention(RetentionPolicy.RUNTIME)
@Target(ElementType.METHOD)
public @interface ExceptionTest {
  /**
   * The exception that the annotated test method must throw
   * in order to pass. (The test is permitted to throw any
   * subtype of the type described by this class object.)
   */
  Class<? extends Exception> value();
}
