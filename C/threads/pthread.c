#include <stdio.h>
#include <stdlib.h>
#include <pthread.h>

void* print_message_function(void*);

int main() {
    pthread_t thread1, thread2;
    const char* message1 = "thread 1";
    const char* message2 = "thread 2";
    int iret1, iret2;

    // Create independent threads each of which will execute function
    iret1 = pthread_create(&thread1, NULL, print_message_function, (void*)message1);
    if (iret1) {
        fprintf(stderr,"error: pthread_create() return code: %d\n", iret1);
        exit(EXIT_FAILURE);
    }

    iret2 = pthread_create(&thread2, NULL, print_message_function, (void*)message2);
    if (iret2) {
        fprintf(stderr,"error: pthread_create() return code: %d\n", iret2);
        exit(EXIT_FAILURE);
    }

    printf("main:                       %d <- pthread_create() for thread 1\n", iret1);
    printf("main:                       %d <- pthread_create() for thread 2\n", iret2);

    // Wait till threads are complete before main continues. Unless we
    // wait we run the risk of executing an exit which will terminate
    // the process and all threads before the threads have completed.

    pthread_join(thread1, NULL);
    pthread_join(thread2, NULL);

    exit(EXIT_SUCCESS);
}
 
void* print_message_function(void* ptr) {
    char* message;
    message = (char*)ptr;
    printf("print_message_function:     %s\n", message);
    return NULL;
}
