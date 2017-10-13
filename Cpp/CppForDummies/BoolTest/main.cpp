#include <cstdio>
#include <cstdlib>
#include <iostream>

using namespace std;

int main(int nNumberofArgs, char* pszArgs[])
{
    //set output format for bool vars to "true" or "false"
    cout.setf(cout.boolalpha);

    //input two values
    int nArg1;
    cout << "Input value 1: ";
    cin >> nArg1;

    int nArg2;
    cout << "Input value 2: ";
    cin >> nArg2;

    //compare the two variables and store the results
    bool b;
    b = nArg1 == nArg2;

    cout << "The statement '" << nArg1 << " equals " << nArg2 << "' is " << b << endl;

    cout << " 0173 = " << 0x7b;

    return 0;
}
