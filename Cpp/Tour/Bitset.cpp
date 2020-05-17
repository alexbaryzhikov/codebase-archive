#include <bitset>
#include <iostream>
#include <string>

std::string bin(int i) { return std::bitset<sizeof(int) * 8>{i}.to_string(); }

int main() { std::cout << bin(388) << std::endl; }