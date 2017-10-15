//
// Factorial Exception
//
#include <cstdio>
#include <cstdlib>
#include <iostream>

using namespace std;

void fn1();
void fn2();
void fn3();

struct Obj
{
    char label;
    Obj(char c) : label(c)
    {
        cout << "Constructing object " << label << "\n";
    }
    ~Obj()
    {
        cout << "Destructing object " << label << "\n";
    }
};

int main()
{
    fn1();
    system("PAUSE");
    return 0;
}

void fn1()
{
    Obj a('a');
    try
    {
        Obj b('b');
        fn2();
    }
    catch (double) { cout << "Double catch!\n"; }
    catch (int) { cout << "Integer catch!\n"; }
    catch (...) { cout << "No can do\n"; }
}

void fn2()
{
    try
    {
        Obj c('c');
        fn3();
    }
    catch (...) { cout << "Wait, what?\n"; throw; }
}

void fn3()
{
    Obj d('d');
    throw 10;
}
