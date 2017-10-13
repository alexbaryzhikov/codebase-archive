//
// Multiple Inheritance
//
#include <cstdio>
#include <cstdlib>
#include <iostream>
using namespace std;

class Bed
{
public:
    Bed() {}
    void sleep() { cout << "Sleep\n"; }
    int weight;
};

class Sofa
{
public:
    Sofa() {}
    void watchTV() { cout << "Watch TV\n"; }
    int weight;
};

class SleeperSofa : public Bed, public Sofa
{
public:
    SleeperSofa() {}
    void foldOut() { cout << "Fold out\n"; }
};

int main()
{
    SleeperSofa mySofa;
    mySofa.watchTV();
    mySofa.foldOut();
    mySofa.sleep();
    system("PAUSE");
    return 0;
}
