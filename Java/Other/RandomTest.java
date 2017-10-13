import java.util.Random;

public class RandomTest {
    public static void main(String[] args) {
        int i, rnd;
        long startTime, endTime, duration;

        // Math.random()
        startTime = System.nanoTime();
        for (i = 0; i < 100000; i++)
            rnd = (int) (Math.random() * 100);
        endTime = System.nanoTime();
        duration = endTime - startTime;
        System.out.printf("%20s %d\n", "Math.random():", duration);

        // util.Random()
        startTime = System.nanoTime();
        Random randint = new Random();
        for (i = 0; i < 100000; i++)
            rnd = randint.nextInt(100);
        endTime = System.nanoTime();
        duration = endTime - startTime;
        System.out.printf("%20s %d\n", "util.Random():", duration);
    }
}
