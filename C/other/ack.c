/*
Ackermann function with caching
*/
#include <stdio.h>
#include <stdlib.h>
#include <string.h>
#include <sys/resource.h>

int m_bits, n_bits;
int *ltable;

int ack( int m, int n ) {
	int idx, res;

	if ( !m ) return n + 1;

	// check if the value of n is in lookup table
	if ( n >= 1 << n_bits ) {
		printf( "LTable miss: m = %d, n = %d\n", m, n );
		idx = 0;
	} else {
		idx = ( m << n_bits ) + n;
		if ( ltable[idx] ) return ltable[idx];
	}

	if ( !n ) res = ack( m - 1, 1 );
	else      res = ack( m - 1, ack( m, n - 1 ) );

	if ( idx ) ltable[idx] = res; // save value to lookup table
	return res;
}

int main( int argc, char **argv ) {

	// increase stack limit
	struct rlimit 	limit;
	unsigned long  	kStackSize = 4294967295; // min stack size in bytes
	int 			result;
	getrlimit( RLIMIT_STACK, &limit );
	if ( limit.rlim_cur < kStackSize ) {
		limit.rlim_cur = kStackSize;
		result = setrlimit( RLIMIT_STACK, &limit );
		if ( result ) {
			fprintf( stderr, "setrlimit returned result = %d\n", result );
		}
	}

	// get arguments
	int m = atoi( argv[1] );
	int n = atoi( argv[2] );

	// setup lookup table memory
	m_bits = 3;
	n_bits = 20; // allows saving n values up to 1 << n_bits
	int ltableSize = sizeof( int ) * ( 1 << ( m_bits + n_bits ) );
	ltable = malloc( ltableSize );
	memset( ltable, 0, ltableSize );

	// run Ackermann
	printf( "Ackermann ( %d, %d ) is %d\n", m, n, ack( m, n ) );

	// dump lookup table to file
	FILE *fp;
	fp = fopen( "ack_ltable.dmp", "wb+" );
	fwrite( ltable, 1, ltableSize / 4, fp );
	fclose( fp );

	return 0;
}
