import java.util.stream.IntStream;

public class Squares3 {
    public static void main(String[] args) {
        /* Version 1 */
        System.out.println("Generate closed range");
        IntStream.rangeClosed(1, 10).map(i -> i * i).forEach(x -> System.out.print(x + " "));
        System.out.println();

        /* Version 2 */
        System.out.println("Generate infinite stream");
        IntStream myStream = IntStream.iterate(1, i -> i + 1);
        myStream.limit(10).map(i -> i * i).forEach(x -> System.out.print(x + " "));
        System.out.println();

        /* Version 3 */
        System.out.println("Generate infinite stream, no map()");
        IntStream myStream2 = IntStream.iterate(1, i -> ((int) Math.pow(Math.sqrt(i) + 1, 2)));
        myStream2.limit(10).forEach(x -> System.out.print(x + " "));
        System.out.println();
    }
}
