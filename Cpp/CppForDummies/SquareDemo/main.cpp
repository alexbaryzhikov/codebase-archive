#include <cstdio>
#include <cstdlib>
#include <iostream>
using namespace std;

double square(double doubleVar)
{
    return doubleVar * doubleVar;
}

void displayExplanation()
{
    cout << "This program sums the square of multiple\n"
         << "series of numbers. Terminate each sequence\n"
         << "by entering a negative number.\n"
         << "Terminate the series by entering an\n"
         << "empty sequence. \n" << endl;
         return;
}

double sumSquareSequence()
{
    double accumulator = 0.0;
    for(;;)
    {
        double dValue = 0.0;
        cout << "Enter next number: ";
        cin >> dValue;

        if (dValue < 0.0)
        {
            break;
        }

        double value = square(dValue);
        accumulator += value;
    }
    return accumulator;
}

int main()
{
    displayExplanation();
    for(;;)
    {
        cout << "Enter next sequence" << endl;
        double accumulatedValue = sumSquareSequence();

        if (accumulatedValue <= 0.0)
        {
            break;
        }

        cout << "\nThe total of the value squared is "
             << accumulatedValue << endl << endl;
    }

}
