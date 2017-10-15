//
//  Constructing character roster as a linked list
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
        pNext = nullptr;
    }
    ~CharStats()
    {
        cout << "Destructing character" << endl;
    }
protected:
    int strn, agil, intl;
    CharStats* pNext;
};

class Roster
{
public:
    Roster()
    {
        cout << "Constructing new character roster" << endl;
        pHead = nullptr;
        pChar = nullptr;
        charsNumber = 0;
    }
    ~Roster()
    {
        cout << "Destructing character roster" << endl;
        if(!pHead) { return; }
        for( ;; )
        {
            pChar = pHead;
            if(!pChar->pNext)
            {
                delete pChar;
                break;
            }
            pHead = pChar->pNext;
            delete pChar;
        }
    }
    void addChar(int s, int a, int i)
    {
        // Adding new character to the bottom of the list
        charsNumber++;
        cout << "Adding character " << charsNumber << " with STR=" <<
            s << " AGI=" << a << " INT=" << i <<  endl;
        if(!pHead)
        {
            pHead = new CharStats;
            pChar = pHead;
        }
        else
        {
            pChar = pHead;
            while(pChar->pNext) { pChar = pChar->pNext; }
            pChar->pNext = new CharStats;
            pChar = pChar->pNext;
        }
        pChar->strn = s;
        pChar->agil = a;
        pChar->intl = i;
    }
    void listChars()
    {
        cout << "--------------------------------" << endl;
        if(!pHead)
        {
            cout << "The roster is empty." << endl;
            cout << "--------------------------------" << endl;
            return;
        }
        cout << "Characters list:" << endl;
        pChar = pHead;
        for( int i = 1; pChar; i++ )
        {
            cout << "Character " << i << ": STR=" << pChar->strn <<
                " AGI=" << pChar->agil << " INT=" << pChar->intl << endl;
            pChar = pChar->pNext;
        }
        cout << "--------------------------------" << endl;
    }
protected:
    CharStats* pChar;
    CharStats* pHead;
    int charsNumber;
};

int main(int nNumberofArgs, char* pszArgs[])
{
    Roster pRost;
    pRost.addChar(5, 3, 4);
    pRost.addChar(2, 1, 7);
    pRost.addChar(3, 6, 2);
    pRost.addChar(1, 1, 1);
    pRost.addChar(99, 4, 0);
    pRost.listChars();

    cout << "\nPress ENTER to return from main()" << endl;
    cin.get();
    return 0;
}
