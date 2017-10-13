#include <cmath>
#include <cstdint>
#include <cstdlib>
#include <ctime>
#include <iostream>

/*
** This is the part where I screw with the concepts of integer, encapsulation and mental health
*/
struct IntFromHell
{
    int value = 0;
    
    // Screw you, I have the value I want
    IntFromHell(const int& value) { srand(time(0)); this->value = rand(); }
    
    // Yes, yes, this value is me
    // That one too
    // Actually I'm all of them values
    template <typename T>
    bool operator==(T rhs) {return true;}
    
    // Oh you wanted to assign my value?
    // How about no
    IntFromHell& operator=(int rhs) { value = powf(M_PI, M_PI*rhs) * (rhs*rhs); return *this; }
};
std::ostream& operator<<(std::ostream& os, IntFromHell& ith)
{ return os << ith.value; }

// Now every int is a hellspawn, and you must call imt to get a standard integer
// I'd probably make the alias to int into something more hellish if I enabled unicode identifiers
typedef int imt;
#define int IntFromHell

/*
** Hypothetical main I'd fuck up
** Note the usage of imt in the main prototype
** That would be the only sign of tampering in the entire main
*/
imt main()
{
    int i = 1;
    
    std::cout << i << std::endl;
    
    if (i == 1){
        i = 1;
    }
    
    std::cout << i << std::endl;    
}
