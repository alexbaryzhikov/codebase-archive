//
//  NameDataSet - linked list class that stores names
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
using namespace std;

class NameDataSet
{
protected:
    string                  sName;
    static NameDataSet*     pHead;
    NameDataSet*            pNext;
public:
    NameDataSet(string& nm) :
        sName(nm), pNext(nullptr) {}
    void add()
    {
        this->pNext = pHead;
        pHead = this;
    }
    static NameDataSet* first() { return pHead; }
    NameDataSet* next() { return pNext; }
    const string& name() { return sName; }
};
NameDataSet* NameDataSet::pHead = nullptr;

int main(int nNumberofArgs, char* pszArgs[])
{
    string refName;
    NameDataSet* pNameObj;
    for(;;)
    {
        cout << "> enter thou name (\"scott\" to complete)\n> ";
        cin >> refName;
        if(refName == "scott") break;
        pNameObj = new NameDataSet(refName);
        pNameObj->add();
    }
    cout << "\n> behold the list of names\n";
    pNameObj = NameDataSet::first();
    while(pNameObj)
    {
        cout << pNameObj->name() << '\n';
        pNameObj = pNameObj->next();
    }
    cout << "\nPress ENTER to return from main()";
    cin.get();
    return 0;
}
