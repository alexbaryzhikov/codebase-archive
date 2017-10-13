#include <pthread.h>
#include <stdio.h>
#include <stdlib.h>

#define COUNT_DONE  10
#define COUNT_HALT1  3
#define COUNT_HALT2  6

pthread_mutex_t count_mutex     = PTHREAD_MUTEX_INITIALIZER;
pthread_cond_t  condition_var   = PTHREAD_COND_INITIALIZER;

void* fn_count1(void*);
void* fn_count2(void*);
int count;

int main() {
    pthread_t thread1, thread2;

    pthread_create(&thread1, NULL, fn_count1, NULL);
    pthread_create(&thread2, NULL, fn_count2, NULL);

    pthread_join(thread1, NULL);
    pthread_join(thread2, NULL);

    printf("main:                  count: %d\n", count);

    exit(EXIT_SUCCESS);
}

/* Write numbers 1-3 and 8-10 as permitted by functionCount2() */
void* fn_count1(void* arg) {
    loop:
    // Lock mutex and then wait for signal to relase mutex
    pthread_mutex_lock(&count_mutex);
    // Wait while functionCount2() operates on count
    // mutex unlocked if condition varialbe in functionCount2() signaled.
    pthread_cond_wait(&condition_var, &count_mutex);
    printf("fn_count1:             count <- %d\n", ++count);
    pthread_mutex_unlock(&count_mutex);

    if(count >= COUNT_DONE)
        return NULL;

    goto loop;
}

/* Write numbers 4-7 */
void* fn_count2(void* arg) {
    loop:
    pthread_mutex_lock(&count_mutex);
    if (count < COUNT_HALT1 || count > COUNT_HALT2)
        // Condition of if statement has been met.
        // Signal to free waiting thread by freeing the mutex.
        // Note: functionCount1() is now permitted to modify "count".
        pthread_cond_signal(&condition_var);
    else
        printf("fn_count2:             count <- %d\n", ++count);
    pthread_mutex_unlock(&count_mutex);

    if (count >= COUNT_DONE)
        return NULL;

    goto loop;
}
