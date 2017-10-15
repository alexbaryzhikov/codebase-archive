#include <cstdio>
#include <cstdlib>
#include <iostream>

using namespace std;

// displayExplanation - prompt the user as to the rules of the game
void displayExplanation(void)
{
    cout << "This program sums multiple series\n"
         << "of numbers. Terminate each sequence\n"
         << "be entering a negative number.\n"
         << "Terminate the series by entering two\n"
         << "negative numbers in a row\n";
    return;
}

// sumSequence - add a sequence of numbers entered from the keyboard
//              until the user enters a negative number.
//              return - the summation of numbers entered
int sumSequence(void)
{
    int accumulator = 0;
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
    return accumulator;
}

int bonusHP(int points)
{
	//const int goldBonus = 400;
	//const int silverBonus = 140;
	if (points > 0) && (points < 7)
    {
        return roundInt((silverBonus/7)*points);
    }
	if (points >= 7) && (points < 39)
    {
		return roundInt(silverBonus+(points-7)*(goldBonus-silverBonus)/32);
    }
	if (points >= 39)
    {
		return goldBonus+(points-39)*2;
    }
	return 0;
}

int main()
{
    displayExplanation();

    for(;;)
    {
        cout << "Enter next sequence\n";
        int accumulatedValue = sumSequence();


        if (accumulatedValue == 0)
        {
            break;
        }

        cout << "The total is " << accumulatedValue << endl << endl;
    }

    return 0;
}
