import java.util.stream.IntStream;
import java.util.function.IntSupplier;

public class SquaresGenerator {
    private static class SqSupplier implements IntSupplier {
        int i = 0;

        @Override
        public int getAsInt() {
            i++;
            return i * i;
        }
    }

    public static void main(String[] args) {
        SqSupplier sqSupplier = new SqSupplier();
        IntStream myStream = IntStream.generate(sqSupplier);
        IntStream myStream2 = IntStream.generate(sqSupplier);

        myStream.limit(10).forEach(x -> System.out.print(x + " "));
        myStream2.limit(10).forEach(x -> System.out.print(x + " "));
        System.out.println();
    }
}
