//
//  STLString
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
using namespace std;

string deleteSpaces(const string &str)
{
    string s = str;
    size_t offset;
    while((offset = s.find(" ")) != string::npos)
    {
        s.erase(offset, 1);
    }
    return s;
}

string insertString(const string &str)
{
    string s = str;
    size_t offset;
    if ((offset = s.find("<ip>")) != string::npos)
    {
        s.erase(offset, 4);
        s.insert(offset, "Coda");
    }
    return s;
}

int main()
{
    cout << "string1 + string2 = " << string("string 1")+string("string 2") << endl;
    char s[256];
    cout << "Enter string with spaces\n> ";
    scanf("%256[^\n]", &s);
    fflush(stdin);
    cout << "Deleting spaces...\n" << deleteSpaces(s) << endl;
    cout << "Enter string with <ip>\n> ";
    fgets(s, 256, stdin);
    cout << "Replacing <ip>...\n" << insertString(s) << endl;

    system("PAUSE");
    return 0;
}

