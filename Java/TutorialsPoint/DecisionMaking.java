public class DecisionMaking {
    public static void main(String[] args) {
        int x = 30, y = 10;
        char grade = 'C';

        /* if ... else */
        if (x == 10)
            System.out.println("Value of X is 10");
        else if (x == 20)
            System.out.println("Value of X is 20");
        else if (x == 30)
            System.out.println("Value of X is 30");
        else
            System.out.println("This is else statement");


        /* nested ifs */
        if (x == 30)
            if (y == 10)
                System.out.println("X = 30 and Y = 10");


        /* switch */
        switch (grade) {
        case 'A' :
            System.out.println("Excellent!");
            break;
        case 'B' :
            /* fall through */
        case 'C' :
            System.out.println("Well done");
            break;
        case 'D' :
            System.out.println("You passed");
            /* fall through */
        case 'F' :
            System.out.println("Better try again");
            break;
        default :
            System.out.println("Invalid grade");
            break;
        }
        System.out.println("Your grade is " + grade);
    }
}
