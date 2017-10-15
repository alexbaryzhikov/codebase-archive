// This program makes .backup copy of a file
#include <iostream>
#include <fstream>
#include <cstdio>
#include <cstdlib>

using namespace std;

int main(int noArgs, char *pArgs[])
{
    for (int i=1; i<noArgs; ++i)
    {
        string  sourceName = pArgs[i],
                targetName = sourceName+".backup";
        ifstream sourceFile(sourceName.c_str(), ios_base::in|ios_base::binary);
        ofstream targetFile(targetName.c_str(), ios_base::out|ios_base::trunc|ios_base::binary);
        if (sourceFile.good() && targetFile.good())
        {
            cout << "Making a backup copy of " << sourceName << "... ";
            char buffer[4096];
            while (!sourceFile.eof() && sourceFile.good())
            {
                sourceFile.read(buffer, 4096);
                targetFile.write(buffer, sourceFile.gcount());
            }
            cout << "finished." << endl;
        }
        else cerr << "Couldn't copy " << sourceName << endl;
    }
    system("PAUSE");
    return 0;
}
