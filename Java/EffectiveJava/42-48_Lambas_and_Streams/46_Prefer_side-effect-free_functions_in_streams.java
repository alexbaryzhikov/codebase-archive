// Uses the streams API but not the paradigm -- Don't do this!
Map<String, Long> freq = new HashMap<>();
try (Stream<String> words = new Scanner(file).tokens()) {
  words.forEach(word -> {
    freq.merge(word.toLowerCase(), 1L, Long::sum);
  });
}


// Proper use of streams to initialize a frequency table
Map<String, Long> freq;
try (Stream<String> words = new Scanner(file).tokens()) {
  freq = words.collect(groupingBy(String::toLowerCase, counting()));
}


// NOTE The forEach operation should be used only to report the result of a stream computation,
// not to perform the computation.


// Pipeline to get a top-ten list of words from a frequency table
List<String> topTen = freq.keySet().stream()
  .sorted(comparing(freq::get).reversed())
  .limit(10)
  .collect(toList());


// NOTE It is customary and wise to statically import all members of Collectors because it makes
// stream pipelines more readable.


// Using a toMap collector to make a map from string to enum
private static final Map<String, Operation> stringToEnum =
  Stream.of(values()).collect(toMap(Object::toString, e -> e));


// Collector to generate a map from key to chosen element for key
Map<Artist, Album> topHits =
  albums.collect(toMap(Album::artist, a -> a, maxBy(comparing(Album::sales))));
// maps each artist to best-selling album


// Collector to impose last-write-wins policy
toMap(keyMapper, valueMapper, (v1, v2) -> v2)


// Grouping collector with classifier function
words.collect(groupingBy(word -> alphabetize(word)))


// Grouping collector with classifier function and downstream collector
Map<String, Long> freq = words.collect(groupingBy(String::toLowerCase, counting()));


// NOTE There is never a reason to say collect(counting()). (Use Stream::count method directly.)
