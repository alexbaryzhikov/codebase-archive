#include <stdio.h>
#include <stdlib.h>
#include <iostream>
using namespace std;

int main()
{
    const char* szString = "Randy";
    cout << "The array is '" << szString << "'" << endl;

    cout << "Display the string as an array: ";
    for(int i = 0; i < 5; i++)
    {
        cout << *szString;
        szString++;
    }
    cout << endl;
    szString -= 5;

    cout << "Display string using a pointer: ";
    const char* pszString = szString;
    pszString += 2;
    cout << pszString << endl;
/*
    while(*pszString)
    {
        cout << *pszString;
        pszString++;
    }
    cout << endl;
*/
}
