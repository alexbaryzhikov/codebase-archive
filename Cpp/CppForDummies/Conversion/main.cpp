#include <cstdio>
#include <cstdlib>
#include <iostream>

using namespace std;

int main(int nNumberofArgs, char* pszArgs [])
{
    cout << "Enter temp Celsius: ";
    int celsius;
    cin >> celsius;

    int factor = 212 - 32;
    int fahrenheit = factor * celsius/100 +32;

    cout << "Fahrenheit: " << fahrenheit << endl;
    cout << "Press Enter..." << endl;
    cin.ignore(10, '\n');
    cin.get();
    return 0;
}
