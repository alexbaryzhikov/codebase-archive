#include <cstdlib>
#include <iostream>
#include <vector>
#include <string>

using namespace std;

template<typename C, typename V>
vector<Value_type<C>*> find_all( C& c, V v ) {
    // find all accurences of v in c
    vector<Value_type<C>*> res;
    for( auto& x : c )
        if ( x == v )
            res.push_back( &x );
    return res;
}

int main()
{
    string m{"Mary had a little lamb"};
    for( const auto p : find_all( m, 'a' ) )  // p is a char*
        if( *p != 'a' )
            cerr << "String bug!\n";
        else
            cout << "Ok.\n";
    return 0;
}
