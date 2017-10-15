#include <cstdio>
#include <cstdlib>
#include <iostream>
using namespace std;

void concatString(char szTarget[], const char szSource[]);

int main()
{
    char szString1[256];
    cout << "Enter string #1:";
    cin.getline(szString1, 128);

    char szString2[128];
    cout << "Enter string #2:" ;
    cin.getline(szString2,125);

    concatString(szString1, " - ");
    concatString(szString1, szString2);

    cout << "\n" << szString1 << endl;

    system("PAUSE");
    return 0;
}

void concatString(char szTarget[], const char szSource[])
{
    int targetIndex = 0;
    while(szTarget[targetIndex])
    {
        targetIndex++;
    }

    int sourceIndex = 0;
    while(szSource[sourceIndex])
    {
        szTarget[targetIndex] = szSource[sourceIndex];
        targetIndex++;
        sourceIndex++;
    }

    szTarget[targetIndex] = '\0';
}

