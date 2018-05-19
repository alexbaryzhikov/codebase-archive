// NOTE To capture the failure, the detail message of an exception should contain the values of all
// parameters and fields that “contributed to the exception.”


/**
 * Construct an IndexOutOfBoundsException.
 *
 * @param lowerBound the lowest legal index value.
 * @param upperBound the highest legal index value plus one.
 * @param index the actual index value.
 */
public IndexOutOfBoundsException(int lowerBound, int upperBound, int index) {
  // Generate a detail message that captures the failure
  super("Lower bound: " + lowerBound + ", Upper bound: " + upperBound + ", Index: " + index);

  // Save failure information for programmatic access
  this.lowerBound = lowerBound;
  this.upperBound = upperBound;
  this.index = index;
}
