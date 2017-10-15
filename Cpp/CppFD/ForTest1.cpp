#include <stdio.h>
#include <stdlib.h>
#include <iostream>

using namespace std;

int main()
{
    // input the loop count
    int nLoopCount;
    cout << "Enter loop count: ";
    cin >> nLoopCount;

    // now loop that many times
    for (int i = 1; i++ <= nLoopCount;)
    {
        cout << "Step " << i - 1 << endl;
    }

    return 0;
}
