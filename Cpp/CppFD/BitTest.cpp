#include <stdio.h>
#include <stdlib.h>
#include <iostream>

using namespace std;

int BitTest() {
    // set output format to hexadecimal
    cout.unsetf(ios::dec);
    cout.setf(ios::hex);

    // initialize two arguments
    int nArg1 = 0x78ABCDEF;
    int nArg2 = 0x12345678;

    // now perform each operation in turn
    // first the unary NOT operator
    cout << " nArg1 = 0x" << nArg1 << endl;
    cout << "~nArg1 = 0x" << ~nArg1 << "\n" << endl;
    cout << " nArg2 = 0x" << nArg2 << endl;
    cout << "~nArg2 = 0x" << ~nArg2 << "\n" << endl;

    // not binary operations
    cout << "  0x" << nArg1 << "\n"
         << "& 0x" << nArg2 << "\n"
         << "  ----------" << "\n"
         << "  0x" << (nArg1 & nArg2) << "\n"
         << endl;

    cout << "  0x" << nArg1 << "\n"
         << "| 0x" << nArg2 << "\n"
         << "  ----------" << "\n"
         << "  0x" << (nArg1 | nArg2) << "\n"
         << endl;

    cout << "  0x" << nArg1 << "\n"
         << "^ 0x" << nArg2 << "\n"
         << "  ----------" << "\n"
         << "  0x" << (nArg1 ^ nArg2) << "\n"
         << endl;

    return 0;
}

int main()
{
    BitTest();
    return 0;
}
