#include <cstdio>
#include <cstdlib>
#include <iostream>

//#define SQUARE(X) X * X
#ifndef SQUARE
//#define SQUARE(X) ((X) * (X))
#ifndef SQUARE
    inline int SQUARE(int X) {return X * X;}
#endif
#endif
using namespace std;

int main()
{
    int i = 3;
    cout << "i = " << i << endl;
    int nSQ = SQUARE(i++);
    cout << "SQUARE(i++) = " << nSQ << endl;
    cout << "i = " << i << endl;

    return 0;
}
