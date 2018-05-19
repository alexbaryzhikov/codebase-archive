// NOTE Given the difficulty of using wait and notify correctly, you should use the higher-level
// concurrency utilities instead.

// NOTE It is impossible to exclude concurrent activity from a concurrent collection; locking it
// will have no effect.


// Concurrent canonicalizing map atop ConcurrentMap - not optimal
private static final ConcurrentMap<String, String> map = new ConcurrentHashMap<>();

public static String intern(String s) {
  String previousValue = map.putIfAbsent(s, s);
  return previousValue == null ? s : previousValue;
}


// Concurrent canonicalizing map atop ConcurrentMap - faster!
public static String intern(String s) {
  String result = map.get(s);
  if (result == null) {
    result = map.putIfAbsent(s, s);
    if (result == null) {
      result = s;
    }
  }
  return result;
}


// NOTE Use ConcurrentHashMap in preference to Collections.synchronizedMap or Hashtable.


// Simple framework for timing concurrent execution
public static long time(Executor executor, int concurrency, final Runnable action)
    throws InterruptedException {

  final CountDownLatch ready = new CountDownLatch(concurrency);
  final CountDownLatch start = new CountDownLatch(1);
  final CountDownLatch done = new CountDownLatch(concurrency);

  for (int i = 0; i < concurrency; i++) {
    executor.execute(new Runnable() {
      public void run() {
        ready.countDown(); // Tell timer we're ready
        try {
          start.await(); // Wait till peers are ready
          action.run();
        } catch (InterruptedException e) {
          Thread.currentThread().interrupt();
        } finally {
          done.countDown(); // Tell timer we're done
        }
      }
    });
  }

  ready.await();  // Wait for all workers to be ready
  long startNanos = System.nanoTime();
  start.countDown(); // And they're off!
  done.await();  // Wait for all workers to finish
  return System.nanoTime() - startNanos;
}


// NOTE For interval timing, always use System.nanoTime in preference to System.currentTimeMillis.


// The standard idiom for using the wait method
synchronized (obj) {
  while (<condition does not hold>) {
    obj.wait(); // (Releases lock, and reacquires on wakeup)
  }
  ... // Perform action appropriate to condition
}


// NOTE Always use the wait loop idiom to invoke the wait method; never invoke it outside of a loop.

// NOTE There is seldom, if ever, a reason to use wait and notify in new code.
