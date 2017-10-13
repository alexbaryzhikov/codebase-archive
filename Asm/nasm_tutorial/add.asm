   SYS_EXIT       equ   1
   SYS_READ       equ   3
   SYS_WRITE      equ   4
   STDIN          equ   0
   STDOUT         equ   1

segment .data

   msg1           db    "Enter a digit: "
   len1           equ   $ - msg1

   msg2           db    "Enter another digit: "
   len2           equ   $ - msg2

   msg3           db    "The sum is: "
   len3           equ   $ - msg3


segment .bss

   num1           resb  2
   num2           resb  2
   rem            resw  1 ; for printing
   outc           resb  2 ; for printing

section .text
   global _start

_start:
   mov   eax, SYS_WRITE
   mov   ebx, STDOUT
   mov   ecx, msg1
   mov   edx, len1
   int   0x80

   mov   eax, SYS_READ
   mov   ebx, STDIN
   mov   ecx, num1
   mov   edx, 2
   int   0x80

   mov   eax, SYS_WRITE
   mov   ebx, STDOUT
   mov   ecx, msg2
   mov   edx, len2
   int   0x80

   mov   eax, SYS_READ
   mov   ebx, STDIN
   mov   ecx, num2
   mov   edx, 2
   int   0x80

   mov   eax, SYS_WRITE
   mov   ebx, STDOUT
   mov   ecx, msg3
   mov   edx, len3
   int   0x80

   ; moving the first number to eax register and second number to ebx
   ; and subtracting ascii '0' to convert it into a decimal number

   xor   eax, eax
   mov   al, [num1]
   sub   eax, '0'
   xor   ebx, ebx
   mov   bl, [num2]
   sub   ebx, '0'

   ; add eax and ebx
   add   eax, ebx

   cmp   eax, 10
   jl    _L0      ; the result is one digit, just print it

   mov   ebx, 10
   xor   edx, edx
   div   ebx
   ; print digit
   add   eax, '0'
   mov   [outc], al
   mov   [rem], dx
   mov   eax, SYS_WRITE
   mov   ebx, STDOUT
   mov   ecx, outc
   mov   edx, 1
   int   0x80

   xor   eax, eax
   mov   ax, [rem]

_L0:
   ; print last digit
   add   eax, '0'
   mov   [outc], al
   mov   al, 0xa
   mov   [outc+1], al ; append newline
   mov   eax, SYS_WRITE
   mov   ebx, STDOUT
   mov   ecx, outc
   mov   edx, 2
   int   0x80

   mov   eax, SYS_EXIT
   xor   ebx, ebx
   int   0x80
