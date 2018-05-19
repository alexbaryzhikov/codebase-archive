
// NOTE You must override `hashCode` in every class that overrides `equals`.

// NOTE The key provision that is violated when you fail to override hashCode is: equal objects
// must have equal hash codes.

public final class PhoneNumber {

  private final short areaCode;
  private final short prefix;
  private final short lineNumber;

  public PhoneNumber(int areaCode, int prefix, int lineNumber) {
    rangeCheck(areaCode, 999, "area code");
    rangeCheck(prefix, 999, "prefix");
    rangeCheck(lineNumber, 9999, "line number");
    this.areaCode = (short) areaCode;
    this.prefix = (short) prefix;
    this.lineNumber = (short) lineNumber;
  }

  private static void rangeCheck(int arg, int max, String name) {
    if (arg < 0 || arg > max) {
      throw new IllegalArgumentException(name +": " + arg);
    }
  }

  @Override public boolean equals(Object o) {
    if (o == this) {
      return true;
    }
    if (!(o instanceof PhoneNumber)) {
      return false;
    }
    PhoneNumber pn = (PhoneNumber) o;
    return pn.lineNumber == lineNumber && pn.prefix == prefix && pn.areaCode == areaCode;
  }

  // Broken - no hashCode method!
  ...  // Remainder omitted
}

Map<PhoneNumber, String> m = new HashMap<>();
m.put(new PhoneNumber(707, 867, 5309), "Jenny");

// The worst possible legal hash function - never use!
@Override public int hashCode() { return 42; }

// -------------------------------------------------------------------------------------------------

// NOTE A simple recipe for `hashCode`:

// 1. Store some constant nonzero value, say, 17, in an int variable called `result`.

// 2. For each significant field f in your object (each field taken into account by the `equals`
//    method, that is), do the following:
//   a. Compute an int hash code c for the field:
//     i. If the field is a boolean , compute (f ? 1 : 0) .
//     ii. If the field is a byte , char , short , or int , compute (int) f .
//     iii. If the field is a long , compute (int) (f ^ (f >>> 32)) .
//     iv. If the field is a float , compute Float.floatToIntBits(f) .
//     v. If the field is a double , compute Double.doubleToLongBits(f) , and then hash the
//       resulting long as in step 2.a.iii.
//     vi. If the field is an object reference and this class’s equals method compares the field
//       by recursively invoking equals , recursively invoke hashCode on the field. If a more
//       complex comparison is required, compute a "canonical representation" for this field and
//       invoke hashCode on the canonical representation. If the value of the field is null ,
//       return 0 (or some other constant, but 0 is traditional).
//     vii. If the field is an array, treat it as if each element were a separate field. That is,
//       compute a hash code for each significant element by applying these rules recursively, and
//       combine these values per step 2.b. If every element in an array field is significant, you
//       can use one of the Arrays.hashCode methods added in release 1.5.
//   b. Combine the hash code c computed in step 2.a into result as follows:
//     result = 31 * result + c;

// 3. Return result .

// 4. When you are finished writing the hashCode method, ask yourself whether equal instances have
//    equal hash codes. Write unit tests to verify your intuition! If equal instances have unequal
//    hash codes, figure out why and fix the problem.

@Override public int hashCode() {
  int result = 17;
  result = 31 * result + areaCode;
  result = 31 * result + prefix;
  result = 31 * result + lineNumber;
  return result;
}

// Lazily initialized, cached hashCode
private volatile int hashCode; // (See Item 71)

@Override public int hashCode() {
  int result = hashCode;
  if (result == 0) {
    result = 17;
    result = 31 * result + areaCode;
    result = 31 * result + prefix;
    result = 31 * result + lineNumber;
    hashCode = result;
  }
  return result;
}

// NOTE Do not be tempted to exclude significant parts of an object from the hash code computation
// to improve performance.
