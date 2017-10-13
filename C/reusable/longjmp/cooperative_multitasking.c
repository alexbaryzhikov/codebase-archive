/* Cooperative (non-preemptive) multitasking example using setjmp */

#include <setjmp.h>
#include <stdio.h>
#define MAX_LOOPS   5
static int finish_flag = 0;

jmp_buf mainTask, childTask;

void call_with_cushion( void );
void child( void );

int main( void ) {
	if ( !setjmp( mainTask ) ) {
		call_with_cushion(); /* child never returns */ /* yield */
	} /* execution resumes after this "}" after first time that child yields */
	for ( ;; ) {
		printf( "Parent\n" );
		if ( finish_flag ) break;
		if ( !setjmp( mainTask ) ) {
			longjmp( childTask, 1 ); /* yield - note that this is undefined under C99 */
		}
	}
	puts( "Paret cycle is complete" );
}

void call_with_cushion( void ) {
	char space[1000]; /* reserve enough space for main to run */
	space[999] = 1; /* do not optimize array out of existence */
	child();
}

void child( void ) {
	for ( int i = 0; i < MAX_LOOPS; i++ ) {
		printf( "Child loop begin\n" );
		if ( !setjmp( childTask ) ) {
			longjmp( mainTask, 1 ); /* yield - invalidates childTask in C99 */
		}

		printf( "Child loop end\n" );
		if ( !setjmp( childTask) ) {
			longjmp( mainTask, 1 ); /* yield - invalidates childTask in C99 */
		}
	}
	puts( "Child cycle is complte" );
	/* Don't return. Instead we should set a flag to indicate that main()
	   should stop yielding to us and then longjmp( mainTask, 1 ) */
	finish_flag = 1;
	longjmp( mainTask, 1 );
}
