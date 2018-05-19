// NOTE Synchronization is required for reliable communication between threads as well as for
// mutual exclusion.

// NOTE Do not use Thread.stop.


// Broken! - How long would you expect this program to run?
public class StopThread {
  private static boolean stopRequested;

  public static void main(String[] args) throws InterruptedException {
    Thread backgroundThread = new Thread(new Runnable() {
      public void run() {
        int i = 0;
        while (!stopRequested) {
          i++;
        }
      }
    });
    backgroundThread.start();

    TimeUnit.SECONDS.sleep(1);
    stopRequested = true;
  }
}


// VM hoisting:

// transform this code:
while (!done) {
  i++;
}
// into this code:
if (!done) {
  while (true) {
    i++;
  }
}


// Properly synchronized cooperative thread termination
public class StopThread {
  private static boolean stopRequested;

  private static synchronized void requestStop() {
    stopRequested = true;
  }

  private static synchronized boolean stopRequested() {
    return stopRequested;
  }

  public static void main(String[] args) throws InterruptedException {
    Thread backgroundThread = new Thread(new Runnable() {
      public void run() {
        int i = 0;
        while (!stopRequested()) {
          i++;
        }
      }
    });
    backgroundThread.start();

    TimeUnit.SECONDS.sleep(1);
    requestStop();
  }
}


// NOTE Synchronization has no effect unless both read and write operations are synchronized.


// Cooperative thread termination with a volatile field
public class StopThread {
  private static volatile boolean stopRequested;

  public static void main(String[] args) throws InterruptedException {
    Thread backgroundThread = new Thread(new Runnable() {
      public void run() {
        int i = 0;
        while (!stopRequested) {
          i++;
        }
      }
    });
    backgroundThread.start();

    TimeUnit.SECONDS.sleep(1);
    stopRequested = true;
  }
}


// Broken - requires synchronization!
private static volatile int nextSerialNumber = 0;

public static int generateSerialNumber() {
  return nextSerialNumber++;
}


private static final AtomicLong nextSerialNum = new AtomicLong();

public static long generateSerialNumber() {
  return nextSerialNum.getAndIncrement();
}


// NOTE Confine mutable data to a single thread.

// NOTE When multiple threads share mutable data, each thread that reads or writes the data must
// perform synchronization.
