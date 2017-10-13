#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

#define NTHREADS 10

void* thread_fn(void*);

pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
int counter;

int main() {
    pthread_t threads[NTHREADS];
    int i, thread_idx[NTHREADS];

    for (i = 0; i < NTHREADS; i++) {
        thread_idx[i] = i;
        pthread_create(threads + i, NULL, thread_fn, thread_idx + i);
    }

    for (i = 0; i < NTHREADS; i++)
        pthread_join(threads[i], NULL);

    // Now that all threads are complete I can print the final result.
    // Without the join I could be printing a value before all the threads
    // have been completed.
    printf("main:             counter: %d\n", counter);

    exit(EXIT_SUCCESS);
}

void* thread_fn(void* arg) {
    printf("thread_fn:        thread %2d, id 0x%lx\n", *(int*)arg, pthread_self());
    pthread_mutex_lock(&mutex1);
    counter++;
    pthread_mutex_unlock(&mutex1);
    return NULL;
}
