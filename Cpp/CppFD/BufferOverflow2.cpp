//
// Buffer Overflow
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <cstring>
#include <vector>
using namespace std;

char* readString(istream &cin)
{
    vector<char> buffer;
    buffer.reserve(64);
    for (;;)
    {
        char c = cin.get();
        if (c == 0 || cin.eof()) break;
        buffer.push_back(c);
    }
    buffer.push_back('\0');
    char *pBuf = new char[buffer.size()];
    if (pBuf != nullptr) strcpy(pBuf, buffer.data());
    return pBuf;
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
    char *pBuf = readString(fs);
    cout << "We successfully read:\n" << pBuf << endl;

    system("PAUSE");
    return 0;
}
