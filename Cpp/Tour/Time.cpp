#include <chrono>
#include <iostream>

#define current_time_nanos() std::chrono::high_resolution_clock::now()

int main() {
    auto t0 = current_time_nanos();
    std::cout << "hello!" << std::endl;
    auto t1 = current_time_nanos();

    std::cout << std::chrono::duration_cast<std::chrono::microseconds>(t1 - t0).count()
              << std::endl;
}