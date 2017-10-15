//
// Buffer Overflow
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <fstream>
#include <cstring>
using namespace std;

char* readString(istream &cin)
{
    char buffer[64] = {};
    for (int i=0; !cin.eof(); ++i)
    {
        buffer[i] = cin.get();
    }
    char *pBuf = new char[strlen(buffer)+1];
    strcpy(pBuf, buffer);
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
