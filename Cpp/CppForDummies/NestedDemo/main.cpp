#include <cstdio>
#include <cstdlib>
#include <iostream>

using namespace std;

int main()
{

    cout << "This program sums multiple series\n"
         << "of numbers. Terminate each sequence\n"
         << "be entering a negative number.\n"
         << "Terminate the series by entering two\n"
         << "negative numbers in a row\n";

    int accumulator;
    for(;;)
    {
        accumulator = 0;
        cout << "Start the next sequence\n";

        for(;;)
        {
            int nValue = 0;
            cout << "Enter next number: ";
            cin >> nValue;

            if (nValue < 0)
            {
                break;
            }
            accumulator += nValue;
        }

        if (accumulator == 0)
        {
            break;
        }

        cout << "The total for this sequence is "
            << accumulator << endl << endl;
    }

    system("PAUSE");
    return 0;
}
