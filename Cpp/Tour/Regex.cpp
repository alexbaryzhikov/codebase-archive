#include <array>
#include <iostream>
#include <regex>
#include <string>

const std::array<std::string, 2> lines{"TX77845", "DC 20500−0001"};

int main() {
    std::regex pat{R"(\w{2}\s*\d{5}(−\d{4})?)"};

    for (size_t i = 0; i < lines.size(); i++) {
        std::smatch matches;
        if (std::regex_search(lines[i], matches, pat)) {
            std::cout << i << ": " << matches[0] << std::endl;
            if (matches.size() > 1 && matches[1].matched)
                std::cout << "\t: " << matches[1] << std::endl;
        }
    }
}