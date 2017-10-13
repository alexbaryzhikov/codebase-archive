#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <fstream>

using namespace std;

ifstream& openFile()
{
    ifstream *pFile = nullptr;
    string fileName;
    for(;;)
    {
        cout << "Enter file name:\n> ";
        cin >> fileName;
        pFile = new ifstream(fileName.c_str());
        if (pFile->good())
        {
            pFile->seekg(0);
            cerr << "File " << fileName << " opened.\n";
            break;
        }
        cerr << "ERROR: coudn't open file " << fileName << "\n";
        delete pFile;
    }
    return *pFile;
}

int main()
{
    ifstream& fileInt = openFile();
    int inputInt;
    while (!fileInt.eof())
    {
        fileInt >> inputInt;
        if (fileInt.fail()) break;
        cout << inputInt << "\n";
    }
    return 0;
}
