import java.util.Iterator;

public class Squares implements Iterator<Integer> {
    private Integer i;

    Squares(Integer initial) {
        i = initial;
    }

    public Integer next() {
        Integer thisOne = i++;
        return thisOne * thisOne;
    }

    public boolean hasNext() {
        return true;
    }

    public static void main(String[] args) {
        Iterator<Integer> squareGenerator = new Squares(1);

        System.out.println(squareGenerator.next());
        System.out.println(squareGenerator.next());
        System.out.println(squareGenerator.next());
    }
}
