//
// Factorial Exception
//
#include <cstdio>
#include <cstdlib>
#include <iostream>

using namespace std;

long fact(int n)
{
    if (n<0)
    {
        throw string("The argument for fact() is negative.\n");
    }
    long result = 1;
    while (n>0) result *= n--;
    return result;
}

int main()
{
    try
    {
        cout << "The factorial of 3 is " << fact(3) << "\n";
        cout << "The factorial of -4 is " << fact(-4) << "\n";
        cout << "The factorial of 5 is " << fact(5) << "\n";
    }
    catch (string error)
    {
        cout << "ERROR: " << error;
    }
    catch (...)
    {
        cout << "ERROR: No can do.\n";
    }
    system("PAUSE");
    return 0;
}
