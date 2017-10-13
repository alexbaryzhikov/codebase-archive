import java.util.ArrayList;
import java.util.Iterator;
import java.util.List;

public class Squares2 implements Iterator<Integer> {
    private Integer i;

    Squares2() {
        i = 1;
    }

    Squares2(Integer initial) {
        i = initial;
    }

    public Integer next() {
        Integer thisOne = i++;
        return thisOne * thisOne;
    }

    public List<Integer> nextN(int n) {
        List<Integer> l = new ArrayList<>();

        for (int i = 0; i < n; i++)
            l.add(next());
        return l;
    }

    public boolean hasNext() {
        return true;
    }

    public static void main(String[] args) {
        Squares2 squareGenerator = new Squares2();

        squareGenerator.nextN(10).forEach(System.out::println);
    }
}
