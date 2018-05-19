
// NOTE Providing a good toString implementation makes your class much more pleasant to use.

System.out.println("Failed to connect: " + phoneNumber);

// NOTE When practical, the toString method should return all of the interesting information
// contained in the object

// NOTE Whether or not you decide to specify the format, you should clearly document your
// intentions.

/**
 * Returns the string representation of this phone number.
 * The string consists of fourteen characters whose format
 * is "(XXX) YYY-ZZZZ", where XXX is the area code, YYY is
 * the prefix, and ZZZZ is the line number. (Each of the
 * capital letters represents a single decimal digit.)
 *
 * If any of the three parts of this phone number is too small
 * to fill up its field, the field is padded with leading zeros.
 * For example, if the value of the line number is 123, the last
 * four characters of the string representation will be "0123".
 *
 * Note that there is a single space separating the closing
 * parenthesis after the area code from the first digit of the
 * prefix.
 */
@Override public String toString() {
  return String.format("(%03d) %03d-%04d", areaCode, prefix, lineNumber);
}

/**
 * Returns a brief description of this potion. The exact details
 * of the representation are unspecified and subject to change,
 * but the following may be regarded as typical:
 *
 * "[Potion #9: type=love, smell=turpentine, look=india ink]"
 */
@Override public String toString() { ... }

// NOTE Provide programmatic access to all of the information contained in the value returned by
// toString.
