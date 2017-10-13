//
//  Linked List
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
using namespace std;

class Linkable
{
public:
    string sWord;
    Linkable* pNext;
};

Linkable* pHead = nullptr;

void addFront(Linkable* pAdd)
{
    pAdd->pNext = pHead;
    pHead = pAdd;
}

void addBack(Linkable* pAdd)
{
    Linkable* pIter = pHead;
    if (!pHead)
    {
        pAdd->pNext = nullptr;
        pHead = pAdd;
        return;
    }
    while(pIter->pNext)
    {
        pIter = pIter->pNext;
    }
    pIter->pNext = pAdd;
}

Linkable* getData()
{
    string sWord = "/q";
    cout << "New entry> ";
    cin >> sWord;
    if (sWord == "/q")
    {
        return nullptr;
    }
    Linkable* pNew = new Linkable;
    pNew->sWord = sWord;
    pNew->pNext = nullptr;
    return pNew;
}

int main(int nNumberofArgs, char* pszArgs[])
{
    cout << "Enter /q to finish list." << endl;
    Linkable* pNew = nullptr;
    while(pNew = getData())
    {
        addFront(pNew);
    }
    if (!pHead)
    {
        cout << "\nList is empty.\n";
        return 0;
    }
    cout << "\nList entries:" << endl;
    for(Linkable* pOut = pHead; pOut; pOut = pOut->pNext)
    {
        cout << pOut->sWord << endl;
    }

}
