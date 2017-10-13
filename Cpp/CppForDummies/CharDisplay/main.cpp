#include <cstdio>
#include <cstdlib>
#include <iostream>
using namespace std;

void displayCharArray(char charArray[]);

int main()
{
    char charMyName[]{"Stephen"};
    displayCharArray(charMyName);
    cout << endl;
}

void displayCharArray(char charArray[])
{
    for(int i = 0; charArray[i]; i++)
    {
        cout << charArray[i];
    }
    cout << endl;
}
