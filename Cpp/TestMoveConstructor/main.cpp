#include <iostream>
#include <vector>
#include <chrono>

using namespace std;

class Foo {
public:
    vector<unsigned> rep;
    Foo() {}
    Foo( unsigned n ) {
        for( unsigned i = 0; i < n; i++ ) {
            rep.push_back( i );
        }
        cout << "List of size " << n << " is created." << endl;
    }
    // copy constructor
    Foo( const Foo& x ) {
        cout << "Copying..." << endl;
        rep = x.rep;
    }
    void operator=( const Foo& x ) {
        cout << "Copying..." << endl;
        rep = x.rep;
    }
    // move constructor
    Foo( Foo&& x ) {
        cout << "Moving..." << endl;
        rep = x.rep;
        x.rep.clear();
    }
    void operator=( Foo&& x ) {
        cout << "Moving..." << endl;
        rep = x.rep;
        x.rep.clear();
    }

    Foo operator+( const Foo& x ) {
        Foo res;
        unsigned sz = ( x.rep.size() < this->rep.size() ) ? x.rep.size() : this->rep.size();
        for ( unsigned i = 0; i < sz; i++ ) {
            res.rep.push_back( x.rep[i] + this->rep[i] );
        }
        cout << "Sum is calculated." << " Result list size is " << res.rep.size() << endl;
        return res;
    }
    void Cat() {
        cout << "--------------" << endl;
        for( unsigned i = 0; i < rep.size(); i++ ) {
            cout << rep[i] << " ";
        }
        cout << endl << endl;
    }
};

int main()
{
    chrono::time_point<chrono::system_clock> start, finish;
    start = chrono::system_clock::now();

    Foo a( 5 );
    a.Cat();
    Foo b( 10 );
    b.Cat();
    Foo c = a + b;
    c.Cat();

    finish = chrono::system_clock::now();
    chrono::duration<double> elapsed_seconds = finish - start;
    cout << "Elapsed time: " << elapsed_seconds.count() << "s" << endl;

    return 0;
}
