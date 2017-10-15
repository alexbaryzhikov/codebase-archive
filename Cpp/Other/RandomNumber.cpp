#include <iostream>
#include <random>
#include <chrono>
using namespace std;

int main()
{
    const int min = 0, max = 1000;
    typedef std::chrono::high_resolution_clock Time;
    typedef std::chrono::microseconds mcs;
    Time::time_point t = Time::now();    // current time in nanoseconds
    mcs d = std::chrono::duration_cast<mcs>(t.time_since_epoch());    // time span between epoch and current time in microseconds
    double seed = d.count() % 1000000;    // cut the number to the last 6 digits, they are the most random
    cout << "time seed: " << seed << "\n";
    mt19937 rng(seed);    // Mersenne-Twister random number engine used
    uniform_int_distribution<int> uni(min,max);    // guaranteed unbiased distribution

    // generating a set of random numbers
    int random_integer;
    int low = 0, med = 0, high = 0;    // distribution counters
    for( int i = 1; i <= 999; i++)
    {
        random_integer = uni(rng);
        cout << random_integer << "\t";
        if( random_integer < 333 ) low++;
            else if( random_integer < 666 ) med++;
                else high++;
    }
    cout << "\n\nnumbers distribution: " << low << " " << med << " " << high << "\n";
}



