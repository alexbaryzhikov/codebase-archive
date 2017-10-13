/* An example illulstrating using threads.
Requires -pthread linkage.
*/

#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>
#include <assert.h>

#define NUM_THREADS 12

void* perform_work(void*);

int main(int argc, char** argv) {
    pthread_t threads[NUM_THREADS];
    int i, rc, thread_idx[NUM_THREADS];

    // create all threads one by one
    for(i = 0; i < NUM_THREADS; i++) {
        thread_idx[i] = i;
        printf("main:             thread %2d -- creating\n", i);
        rc = pthread_create(threads + i, NULL, perform_work, thread_idx + i);
        assert(!rc);
    }

    // wait for each thread to complete
    for(i = 0; i < NUM_THREADS; i++) {
        // block until thread 'i' completes
        rc = pthread_join(threads[i], NULL);
        assert(!rc);
        printf("main:             thread %2d -- completed\n", i);
    }

    printf("main:             All threads completed successfully\n");
    exit(EXIT_SUCCESS);
}

void* perform_work(void* argument) {
    int passed_in_value;

    passed_in_value = *(int*)argument;
    printf("perform_work:     thread %2d -- started\n", passed_in_value);

    // do very important work
    double ans;
    for (int i = 1; i < 100000; i++)
        for (int j = 1; j < 100000; j++)
            ans = (i + j) / j;

    printf("perform_work:     thread %2d:  ans: %g\n", passed_in_value, ans);

    return NULL;
}
