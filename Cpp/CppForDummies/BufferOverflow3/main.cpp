//
// Buffer Overflow
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <cstring>
using namespace std;

string readString(istream &cin)
{
    string s;
    getline(cin, s, '\0');
    return s;
}

int main()
{
    string fileName;
    cout << "This program reads string from a file.\n";
    cout << "Enter file name:\n> ";
    cin >> fileName;
    ifstream fs(fileName.c_str());
    if (!fs)
    {
        cerr << "Error: No can do";
        exit(-1);
    }
    string s = readString(fs);
    cout << "We successfully read:\n" << s << endl;

    system("PAUSE");
    return 0;
}
