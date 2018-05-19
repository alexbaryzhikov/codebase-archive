// Immutable class that uses defensive copying
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
    this.start = new Date(start.getTime());
    this.end = new Date(end.getTime());
    if (this.start.compareTo(this.end) > 0) {
      throw new IllegalArgumentException(start + " after " + end);
    }
  }

  public Date start () { return new Date(start.getTime()); }

  public Date end () { return new Date(end.getTime()); }

  public String toString() { return start + " - " + end; }

  ... // Remainder omitted
}


// This ugly program would then generate a Period instance whose end precedes its start:
public class BogusPeriod {
  // Byte stream could not have come from real Period instance!
  private static final byte[] serializedForm = new byte[] {
    (byte)0xac, (byte)0xed, 0x00, 0x05, 0x73, 0x72, 0x00, 0x06,
    0x50, 0x65, 0x72, 0x69, 0x6f, 0x64, 0x40, 0x7e, (byte)0xf8,
    0x2b, 0x4f, 0x46, (byte)0xc0, (byte)0xf4, 0x02, 0x00, 0x02,
    0x4c, 0x00, 0x03, 0x65, 0x6e, 0x64, 0x74, 0x00, 0x10, 0x4c,
    0x6a, 0x61, 0x76, 0x61, 0x2f, 0x75, 0x74, 0x69, 0x6c, 0x2f,
    0x44, 0x61, 0x74, 0x65, 0x3b, 0x4c, 0x00, 0x05, 0x73, 0x74,
    0x61, 0x72, 0x74, 0x71, 0x00, 0x7e, 0x00, 0x01, 0x78, 0x70,
    0x73, 0x72, 0x00, 0x0e, 0x6a, 0x61, 0x76, 0x61, 0x2e, 0x75,
    0x74, 0x69, 0x6c, 0x2e, 0x44, 0x61, 0x74, 0x65, 0x68, 0x6a,
    (byte)0x81, 0x01, 0x4b, 0x59, 0x74, 0x19, 0x03, 0x00, 0x00,
    0x78, 0x70, 0x77, 0x08, 0x00, 0x00, 0x00, 0x66, (byte)0xdf,
    0x6e, 0x1e, 0x00, 0x78, 0x73, 0x71, 0x00, 0x7e, 0x00, 0x03,
    0x77, 0x08, 0x00, 0x00, 0x00, (byte)0xd5, 0x17, 0x69, 0x22,
    0x00, 0x78
  };

  public static void main(String[] args) {
    Period p = (Period) deserialize(serializedForm);
    System.out.println(p);
  }

  // Returns the object with the specified serialized form
  private static Object deserialize(byte[] sf) {
    try {
      InputStream is = new ByteArrayInputStream(sf);
      ObjectInputStream ois = new ObjectInputStream(is);
      return ois.readObject();
    } catch (Exception e) {
      throw new IllegalArgumentException(e);
    }
  }
}


// readObject method with validity checking
private void readObject(ObjectInputStream s) throws IOException, ClassNotFoundException {
  s.defaultReadObject();

  // Check that our invariants are satisfied
  if (start.compareTo(end) > 0) {
    throw new InvalidObjectException(start +" after "+ end);
  }
}


public class MutablePeriod {
  // A period instance
  public final Period period;

  // period ' s start field, to which we shouldn ' t have access
  public final Date start;

  // period ' s end field, to which we shouldn ' t have access
  public final Date end;

  public MutablePeriod() {
    try {
      ByteArrayOutputStream bos = new ByteArrayOutputStream();
      ObjectOutputStream out = new ObjectOutputStream(bos);

      // Serialize a valid Period instance
      out.writeObject(new Period(new Date(), new Date()));

      /*
       * Append rogue "previous object refs" for internal
       * Date fields in Period. For details, see "Java
       * Object Serialization Specification," Section 6.4.
       */
      byte[] ref = { 0x71, 0, 0x7e, 0, 5 }; // Ref #5
      bos.write(ref); // The start field
      ref[4] = 4;     // Ref # 4
      bos.write(ref); // The end field

      // Deserialize Period and "stolen" Date references
      ObjectInputStream in = new ObjectInputStream(new ByteArrayInputStream(bos.toByteArray()));
      period = (Period) in.readObject();
      start = (Date)    in.readObject();
      end = (Date)      in.readObject();
    } catch (Exception e) {
      throw new AssertionError(e);
    }
  }
}

// To see the attack in action, run the following program:
public static void main(String[] args) {
  MutablePeriod mp = new MutablePeriod();
  Period p = mp.period;
  Date pEnd = mp.end;

  // Let ' s turn back the clock
  pEnd.setYear(78);
  System.out.println(p);

  // Bring back the 60s!
  pEnd.setYear(69);
  System.out.println(p);
}

// Wed Apr 02 11:04:26 PDT 2008 - Sun Apr 02 11:04:26 PST 1978
// Wed Apr 02 11:04:26 PDT 2008 - Wed Apr 02 11:04:26 PST 1969


// NOTE When an object is deserialized, it is critical to defensively copy any field containing an
// object reference that a client must not possess.


// readObject method with defensive copying and validity checking
private void readObject(ObjectInputStream s) throws IOException, ClassNotFoundException {
  s.defaultReadObject();

  // Defensively copy our mutable components
  start = new Date(start.getTime());
  end = new Date(end.getTime());

  // Check that our invariants are satisfied
  if (start.compareTo(end) > 0) {
    throw new InvalidObjectException(start +" after "+ end);
  }
}


// NOTE Do not use the writeUnshared and readUnshared methods.
