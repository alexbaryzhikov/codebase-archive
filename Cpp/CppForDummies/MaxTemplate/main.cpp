//
//  Max Template
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
using namespace std;

template <typename T> T maximum(T a, T b) { return (a > b) ? a : b; }

int main()
{
    cout << "maximum(-1, 2) = " << maximum(-1, 2) << endl;
    cout.setf(ios_base::fixed);
    cout.precision(1);
    cout << "maximum(4, 2.0) = " << maximum<double>(4, 2.0) << endl;
    system("PAUSE");
    return 0;
}

