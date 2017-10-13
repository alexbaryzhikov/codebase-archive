//
// String Stream - reads strings from the file to the istringstream buffer,
//                 parses it, and outputs through ostringstream
//
// File name to input - accounts.txt
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
#include <sstream>
#include <fstream>
using namespace std;

// string format:
// name, account balance
bool parseString(const char *pString, char *pName, int nameArraySize, long& accountNum, double& balance)
{
    istringstream strPar(pString);
    strPar.getline(pName, nameArraySize, ',');
    strPar >> accountNum;
    strPar >> balance;
    return !strPar.fail();
}

int main()
{
    // preparing the file to parse
    char fileName[128];
    cout << "Enter the name of the file to parse:\n> ";
    cin.getline(fileName, 127);
    ifstream inputFile(fileName);
    if (!inputFile.good())
    {
        cout << "Couldn't open file " << fileName << "\n";
        return 0;
    }

    // parsing lines from the file
    for (int lineNum = 1;; ++lineNum)
    {
        char buffer[256];
        inputFile.getline(buffer, 255);
        if (inputFile.fail()) break;
        cout << "\n" << lineNum << " : " << buffer << "\n";

        char accName[80];
        long accountNum;
        double balance;
        if (!parseString(buffer, accName, 79, accountNum, balance))
        {
            cout << "Couldn't parse the string" << endl;
            continue;
        }

        ostringstream strOut;
        strOut << accName << ", " << balance << " " << accountNum << ends;
        cout << "New reordered string: " << strOut.str() << "\n";
    }
    cout << "\nPress ENTER to continue";
    cin.get();
    return 0;
}
