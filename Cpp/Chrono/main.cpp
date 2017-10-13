// generates 10M random numbers and measures how much time did it take

#include <iostream>
#include <random>
#include <chrono>

int main()
{
    typedef std::chrono::high_resolution_clock Clock;
    typedef std::chrono::duration<double> sec;
    Clock::time_point t0 = Clock::now();
    const int N = 10000000;
    typedef std::minstd_rand G;
    G g;
    typedef std::uniform_int_distribution<> D;
    D d(-57, 365);
    int c = 0;
    for (int i = 0; i < N; ++i)
        c += d(g);
    Clock::time_point t1 = Clock::now();
    std::cout << N/sec(t1-t0).count() << " random numbers per second.\n";
    return c;
}
