#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <iomanip>
using namespace std;

const float pi = 3.14;
float n[] = {10.0, 9.0, 8.0, 7.0, 6.0};

void displayArray(float* pArray, int nSize);

int main()
{
    float* pPtr = n;
    (*pPtr)++;
    displayArray(n, 5);
}

void displayArray(float* pArray, int nSize)
{
    for (int i = 0; i++ < nSize; pArray++)
    {
            cout << fixed << setprecision(1) << "n" << i << " = " << *pArray << endl;
    }

}
