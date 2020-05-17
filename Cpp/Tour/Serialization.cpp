#include <fstream>
#include <iostream>
#include <stdexcept>
#include <string>

struct Entry {
    std::string name;
    int id;
};

std::ostream &operator<<(std::ostream &os, const Entry &e) {
    return os << '{' << '"' << e.name << '"' << ',' << e.id << '}';
}

std::istream &operator>>(std::istream &is, Entry &e) {
    char c;
    if (is >> c && c == '{' && is >> c && c == '"') {
        std::string name;
        while (is.get(c) && c != '"') name += c;

        if (is >> c && c == ',') {
            int id;
            if (is >> id >> c && c == '}') {
                e = Entry{name, id};
                return is;
            }
        }
    }
    is.setstate(std::ios_base::failbit);
    return is;
}

int main() {
    Entry e1{"Jason Bourne", 77};
    Entry e2{"Cliff Booth", 234561};
    std::cout << e1 << std::endl << e2 << std::endl;

    std::ofstream fout{"enries.txt"};
    if (!fout) throw std::runtime_error{"Failed to open 'entries.txt' for write"};
    fout << e1 << '\n' << e2;
    fout.close();

    std::ifstream fin{"enries.txt"};
    if (!fin) throw std::runtime_error{"Failed to open 'entries.txt' for read"};
    Entry e1_rec;
    Entry e2_rec;
    fin >> e1_rec >> e2_rec;
    fin.close();
    std::cout << e1_rec << std::endl << e2_rec << std::endl;
}
