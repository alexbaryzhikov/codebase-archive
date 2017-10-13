#include <fstream>
using namespace std;

int main()
{
    ofstream my("MyName.txt");
    my << "Hello world!" << endl;
    return 0;
}
