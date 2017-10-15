//
//  Testing playground
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <iomanip>
using namespace std;

class DoNothing
{
public:
    int* num;
    DoNothing(int n)
    {
        num = new int(n);
    }
    ~DoNothing()
    {
        delete num;
    }
};

int main()
{
    DoNothing* a = new DoNothing(5);
    cout << *(a->num) << endl;

    double f = 245.105;
    //cout.setf(cout.fixed);
    cout << fixed;
    cout << setprecision(2) << f << '\n';
    cout << setprecision(5) << f << '\n';
    return 0;
}
