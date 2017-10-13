   SYS_EXIT       equ   1
   SYS_READ       equ   3
   SYS_WRITE      equ   4
   STDIN          equ   0
   STDOUT         equ   1

section .data

   msg            db    "The result is: "
   len            equ   $ - msg

segment .bss

   res            resb  2

section  .text
   global _start
   
_start:
   mov   ax, 8
   mov   bl, 2
   div   bl
   
   add   ax, '0'
   mov   [res], ax

   mov   eax, SYS_WRITE
   mov   ebx, STDOUT
   mov   ecx, msg  
   mov   edx, len
   int   0x80
   
   ; append newline
   mov   al, 0xa
   mov   [res+1], al

   mov   eax, SYS_WRITE
   mov   ebx, STDOUT
   mov   ecx, res
   mov   edx, 2
   int   0x80
   
   
   mov   eax, SYS_EXIT
   int   0x80
