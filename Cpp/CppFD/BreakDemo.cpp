#include <cstdio>
#include <cstdlib>
#include <iostream>

using namespace std;

int main()
{
    int accumulator = 0;
    cout << "This program sums values from the user\n"
         << "Terminate by entering zero\n"
         << endl;

    // loop forever
    for(;;)
    {
        int nValue = 0;
        cout << "INPUT> ";
        cin >> nValue;

        if (nValue == 0)
        {
            break;
        }

        if (nValue < 0)
        {
//            break;
            cout << "Error: Negatives are not allowed." << endl;
            continue;
        }

        accumulator += nValue;
    }

    cout << "\nRETURN: " << accumulator << endl;

    return 0;
}
