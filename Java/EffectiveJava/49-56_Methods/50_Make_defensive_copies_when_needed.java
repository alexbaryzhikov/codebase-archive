// NOTE You must program defensively, with the assumption that clients of your class will do their
// best to destroy its invariants.


// Broken "immutable" time period class
public final class Period {
  private final Date start;
  private final Date end;

  /**
  * @param start the beginning of the period
  * @param end the end of the period; must not precede start
  * @throws IllegalArgumentException if start is after end
  * @throws NullPointerException if start or end is null
  */
  public Period(Date start, Date end) {
    if (start.compareTo(end) > 0) {
      throw new IllegalArgumentException(start + " after " + end);
    }
    this.start = start;
    this.end = end;
  }

  public Date start() { return start; }
  public Date end() { return end; }

  ... // Remainder omitted
}


// Attack the internals of a Period instance
Date start = new Date();
Date end = new Date();
Period p = new Period(start, end);
end.setYear(78); // Modifies internals of p!


// NOTE It is essential to make a defensive copy of each mutable parameter to the constructor.


// Repaired constructor - makes defensive copies of parameters
public Period(Date start, Date end) {
  this.start = new Date(start.getTime());
  this.end = new Date(end.getTime());
  if (this.start.compareTo(this.end) > 0) {
    throw new IllegalArgumentException(start +" after "+ end);
  }
}


// NOTE Defensive copies are made before checking the validity of the parameters (Item 38), and
// the validity check is performed on the copies rather than on the originals.


// NOTE Do not use the clone method to make a defensive copy of a parameter whose type is
// subclassable by untrusted parties.


// Second attack on the internals of a Period instance
Date start = new Date();
Date end = new Date();
Period p = new Period(start, end);
p.end().setYear(78); // Modifies internals of p!


// NOTE Return defensive copies of mutable internal fields:


// Repaired accessors - make defensive copies of internal fields
public Date start() { return new Date(start.getTime()); }
public Date end() { return new Date(end.getTime()); }
