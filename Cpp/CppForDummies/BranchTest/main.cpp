#include <stdio.h>
#include <stdlib.h>
#include <iostream>
using namespace std;

int main()
{
    // input the first argument...
    int nArg1;
    cout << "Enter arg1: ";
    cin >> nArg1;

    // ...and the second
    int nArg2;
    cout << "Enter arg2: ";
    cin >> nArg2;

    // now decide what to do:
    if (nArg1 > nArg2)
    {
        cout << "Argument 1 is greater" << endl;
    }
    else
    {
        cout << "Argument 1 is NOT greater" << endl;
    }

    return 0;
}

