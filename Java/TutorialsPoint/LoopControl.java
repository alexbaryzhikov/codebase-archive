public class LoopControl {
    public static void main(String[] args) {
        int i = 0;
        int[] numbers = {10, 20, 30, 40, 50};
        String[] names = {"James", "Larry", "Tom", "Lacy"};

        while (i < 5) {
            System.out.print(i + " ");
            i++;
        }
        System.out.println();

        for (i = 5; i < 10; i++) {
            System.out.print(i + " ");
        }
        System.out.println();

        do {
            System.out.print(i + " ");
            i++;
        } while (i < 15);
        System.out.println();

        for (int x : numbers) {
            if (x == 30)
                break;
            System.out.print(x + " ");
        }
        System.out.println();

        for (int x : numbers) {
            if (x == 30)
                continue;
            System.out.print(x + " ");
        }
        System.out.println();

        for (String name : names) {
            if (name != names[names.length-1])
                System.out.print(name + ", ");
            else
                System.out.println(name);
        }
    }
}
