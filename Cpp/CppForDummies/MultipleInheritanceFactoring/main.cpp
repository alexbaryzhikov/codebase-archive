//
// Multiple Inheritance Factoring
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
using namespace std;

class Furniture
{
public:
    int weight;
    Furniture(int w) : weight(w) {}
};

class Bed : public Furniture
{
public:
    Bed(int w) : Furniture(w) {}
    void sleep() { cout << "Sleep\n"; }
};

class Sofa : public Furniture
{
public:
    Sofa(int w) : Furniture(w) {}
    void watchTV() { cout << "Watch TV\n"; }
};

class SleeperSofa : public Bed, public Sofa
{
public:
    SleeperSofa(int w) : Bed(w), Sofa(w) {}
    void foldOut() { cout << "Fold out\n"; }
};

int main()
{
    SleeperSofa mySofa(10);
//#define TRY
#ifdef TRY
    cout << "Weight = " << mySofa.weight << endl;
#else
    Furniture *pFurn = (Furniture*)(Sofa*)&mySofa;
    cout << "Weight = " << pFurn->weight << endl;
#endif
    system("PAUSE");
    return 0;
}
