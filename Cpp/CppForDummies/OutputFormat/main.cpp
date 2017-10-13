#include <iostream>
#include <cstdlib>
#include <cstdio>

using namespace std;

int main()
{
    cout.setf(cout.left);
    cout.unsetf(cout.right);
    int i = 123;
    cout.fill('+');
    cout << "i = [";
    cout.width(20);
    cout << i << "]" << endl;
    system("PAUSE");
    return 0;
}
