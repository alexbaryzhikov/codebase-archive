public class Arrays {

    /* Process array with "for" loop */
    static void forArray() {
        double[] myList = {1.9, 2.9, 3.4, 3.5};

        // Print all the array elements
        for (int i = 0; i < myList.length; i++)
            System.out.println(myList[i] + " ");

        // Summing all elements
        double total = 0;
        for (int i = 0; i < myList.length; i++)
            total += myList[i];
        System.out.println("Total is " + total);

        // Finding the largest element
        double max = myList[0];
        for (int i = 1; i < myList.length; i++)
            if (myList[i] > max) max = myList[i];
        System.out.println("Max is " + max);  
    }

    /* Process array with "for each" loop*/
    static void forEachArray() {
        double[] myList = {1.9, 2.9, 3.4, 3.5};

        // Print all the array elements
        for (double element: myList)
            System.out.println(element);
    }

    /* Pass an array to a method */
    static void printArray(int[] array) {
        for (int i : array)
            System.out.print(i + " ");
        System.out.println();
    }

    /* Return an array from a method */
    static int[] reverse(int[] list) {
        int[] result = new int[list.length];

        for (int i = 0, j = result.length - 1; i < list.length; i++, j--)
            result[j] = list[i];
        return result;
    }

    public static void main(String[] args) {
        int n = 10;
        
        /* Declaring an array */
        double[] arr0;
        double[] arr1 = new double[n];
        double[] arr2 = { 1.0, 2.0, 3.0 };

        /* Array length*/
        System.out.println("arr1 length is " + arr1.length);

        forArray();
        forEachArray();
        printArray(new int[]{3, 1, 2, 6, 4, 2});
        printArray(reverse(new int[]{1, 2, 3, 4, 5}));
    }
}
