#include <cstdio>
#include <cstdlib>
#include <iostream>

using namespace std;

void multiplyBy2(int& nArg)
{
    nArg *= 2;
}

int main(int nNumberofArgs, char* pszArgs[])
{
    int nValue;
    cout << "Enter number: ";
    cin >> nValue;

    multiplyBy2(nValue);


    cout << "Result: " << nValue << endl;
    // wait until user is ready before terminating program
    // to allow the user to see the program results
    cout << "Press Enter to continue..." << endl;
    cin.ignore(10, '\n');
    cin.get();
    return 0;
}
