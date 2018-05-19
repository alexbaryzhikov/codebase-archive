// Stream-based program to generate the first 20 Mersenne primes
public static void main(String[] args) {
  primes().map(p -> TWO.pow(p.intValueExact()).subtract(ONE))
    .filter(mersenne -> mersenne.isProbablePrime(50))
    .limit(20)
    .forEach(System.out::println);
}

static Stream<BigInteger> primes() {
  return Stream.iterate(TWO, BigInteger::nextProbablePrime);
}


// NOTE Parallelizing a pipeline is unlikely to increase its performance if the source is from
// Stream.iterate, or the intermediate operation limit is used.

// NOTE Do not parallelize stream pipelines indiscriminately.

// NOTE Performance gains from parallelism are best on streams over ArrayList, HashMap, HashSet,
// and ConcurrentHashMap instances; arrays; int ranges; and long ranges.

// NOTE Not only can parallelizing a stream lead to poor performance, including liveness failures;
// it can lead to incorrect results and unpredictable behavior.

// NOTE Under the right circumstances, it is possible to achieve near-linear speedup in the number
// of processor cores simply by adding a parallel call to a stream pipeline.


// Prime-counting stream pipeline - benefits from parallelization
static long pi(long n) {
  return LongStream.rangeClosed(2, n)
    .mapToObj(BigInteger::valueOf)
    .filter(i -> i.isProbablePrime(50))
    .count();
}

// Prime-counting stream pipeline - parallel version
static long pi(long n) {
  return LongStream.rangeClosed(2, n)
    .parallel()
    .mapToObj(BigInteger::valueOf)
    .filter(i -> i.isProbablePrime(50))
    .count();
}
