; Should be linked with gcc

extern printf
global main

section .data

myint       dd      1234
mystring    db      'This number -> %d <- should be 1234', 0xa, 0

section .text

main:
    push    dword [myint]
    push    dword mystring
    call    printf
    add     esp, byte 8
    ret
