//
//  Least Common Denominator
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
using namespace std;

void lcd(unsigned n)
{
    if (n <= 1)
    {
        cout << endl;
        return;
    }
    int i;
    for (i=2; n%i != 0; ++i) {}
    cout << i << " ";
    lcd(unsigned(n/i));
}

int main()
{
    for(;;)
    {
        unsigned n;
        cout << "Enter integer number (0 or less to terminate)\n> ";
        cin >> n;
        if (n <= 0) break;
        lcd(n);
    }
    system("PAUSE");
    return 0;
}

