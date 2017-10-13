//
//  Constructing character roster as an array
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
using namespace std;

class Roster;

class CharStats
{
    friend class Roster;
public:
    CharStats()
    {
        cout << "Constructing character" << endl;
        strn = 0; agil = 0; intl = 0;
    }
    ~CharStats()
    {
        cout << "Destructing character" << endl;
    }
protected:
    int strn, agil, intl;
};

class Roster
{
public:
    Roster()
    {
        cout << "Constructing new character roster" << endl;
        pChars = new CharStats[3];
    }
    ~Roster()
    {
        cout << "Destructing character roster" << endl;
        delete[] pChars;
    }
    void changeStats(int charNumber, int s, int a, int i)
    {
        CharStats* pCharToChange = pChars + charNumber - 1;
        cout << "Changing stats of character "<< charNumber << endl;
        pCharToChange->strn = s;
        pCharToChange->agil = a;
        pCharToChange->intl = i;
    }
    void listChars()
    {
        CharStats* pCurrentChar = pChars;
        cout << "--------------------------------" << endl;
        cout << "Characters in the roster:" << endl;
        for( int i = 0; i++ < 3; )
        {
            cout << "Character " << i << ": STR=" << pCurrentChar->strn <<
                " AGI=" << pCurrentChar->agil << " INT=" << pCurrentChar->intl << endl;
            pCurrentChar++;
        }
        cout << "--------------------------------" << endl;
    }
protected:
    CharStats* pChars;
};

int main(int nNumberofArgs, char* pszArgs[])
{
    Roster pRost01;
    pRost01.changeStats(1, 5, 3, 4);
    pRost01.changeStats(2, 2, 1, 7);
    pRost01.changeStats(3, 3, 6, 2);
    pRost01.listChars();

    cout << "\nPress ENTER to return from main()" << endl;
    cin.get();
    return 0;
}
