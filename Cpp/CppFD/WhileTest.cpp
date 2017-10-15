#include <stdio.h>
#include <stdlib.h>
#include <iostream>
#include <windows.h>

using namespace std;

int main()
{
    // input the loop count
    double dIncrementStep;
    cout << "Enter increment step: ";
    cin >> dIncrementStep;

    double dContainer = 0.0;
    double dIncrement = 0.0;

    while(true)
    {
        dIncrement += dIncrementStep;
        dContainer += dIncrement;
        cout << dContainer << '\n';
        Sleep(100);
    }

    return 0;
}
