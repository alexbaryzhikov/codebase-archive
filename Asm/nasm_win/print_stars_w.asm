; Doesn't work, cannot interface with Win32 like that (int 0x21)

section .text
    global _WinMain@16

_WinMain@16:            ;tell linker entry point
    mov     eax, 0

    mov     eax, 4      ;system call number (sys_write)
    mov     ebx, 1      ;file descriptor (stdout)
    mov     ecx, msg    ;message to write
    mov     edx, len    ;message length
    int     0x21        ;call kernel

    mov     eax, 4      ;system call number (sys_write)
    mov     ebx, 1      ;file descriptor (stdout)
    mov     ecx, s2     ;message to write
    mov     edx, 10     ;message length
    int     0x21        ;call kernel

    mov     eax, 1      ;system call number (sys_exit)
    int     0x21        ;call kernel

    ret     16

section .data
msg db 'Displaying 9 stars',0xa
len equ $ - msg
s2 times 9 db '*'
db 0xa
