#include <stdio.h>
#include <endian.h>

int main(int argc, char** argv) {

    if (__BYTE_ORDER == __LITTLE_ENDIAN)
        printf("little\n");
    else
        printf("big\n");

}
