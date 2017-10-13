
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <iomanip>
using namespace std;

int main()
{
    double f = 245.105;
    //cout.setf(cout.fixed);
    cout << fixed;
    cout << setprecision(2) << f << '\n';
    cout << setprecision(5) << f << '\n';
    return 0;
}
