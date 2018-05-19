// NOTE Any program that relies on the thread scheduler for correctness or performance is likely
// to be nonportable.

// NOTE Threads should not run if they aren’t doing useful work.


// Awful CountDownLatch implementation - busy-waits incessantly!
public class SlowCountDownLatch {
  private int count;

  public SlowCountDownLatch(int count) {
    if (count < 0) {
      throw new IllegalArgumentException(count + " < 0");
    }
    this.count = count;
  }

  public void await() {
    while (true) {
      synchronized(this) {
        if (count == 0) {
          return;
        }
      }
    }
  }

  public synchronized void countDown() {
    if (count != 0) {
      count--;
    }
  }
}


// NOTE Resist the temptation to “fix” the program by putting in calls to Thread.yield.

// NOTE Thread.yield has no testable semantics.

// NOTE Thread priorities are among the least portable features of the Java platform.
