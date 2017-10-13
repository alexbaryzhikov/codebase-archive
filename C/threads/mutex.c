#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

void* fn_count(void*);
pthread_mutex_t mutex1 = PTHREAD_MUTEX_INITIALIZER;
int counter;

int main() {
    pthread_t thread1, thread2;

    pthread_create(&thread1, NULL, fn_count, NULL);
    pthread_create(&thread2, NULL, fn_count, NULL);

    pthread_join(thread1, NULL);
    pthread_join(thread2, NULL);

    exit(EXIT_SUCCESS);
}

void* fn_count(void* arg) {
    pthread_mutex_lock(&mutex1);
    counter++;
    printf("fn_count:         thread 0x%lx:  counter: %d\n", pthread_self(), counter);
    pthread_mutex_unlock(&mutex1);
    return NULL;
}
