.intel_syntax
.data

L8:
.long 1
.string "
"

L7:
.long 3
.string "Mal"

L6:
.long 1
.string "
"

L5:
.long 4
.string "Bien"

.text

.global _tigermain
.type _tigermain, @function
_tigermain:
push %ebp
mov %ebp, %esp
push %ebx
push %esi
push %edi
sub %esp, 0
L25:
mov %eax, 6
push %eax
push %ebp
call L0
add %esp, 8
mov %ebx, 720
cmp %eax, %ebx
je L9
jmp L10
L10:
mov %eax, offset L7
push %eax
call print
add %esp, 4
mov %eax, offset L8
push %eax
call print
add %esp, 4
mov %ebx, 0
L11:
jmp L24
L9:
mov %eax, offset L5
push %eax
call print
add %esp, 4
mov %eax, offset L6
push %eax
call print
add %esp, 4
mov %ebx, 0
jmp L11
L24:

add %esp, 0
pop %edi
pop %esi
pop %ebx
pop %ebp
ret

.global L0
.type L0, @function
L0:
push %ebp
mov %ebp, %esp
push %ebx
push %esi
push %edi
sub %esp, 0
L15:
mov %edi, [%ebp +12]
mov %ebx, 0
cmp %edi, %ebx
je L1
jmp L2
L2:
mov %eax, [%ebp +12]
mov %ebx, %eax
mov %eax, [%ebp +12]
mov %ecx, 1
sub %eax,%ecx
push %eax
mov %eax, [%ebp +8]
push %eax
call L0
add %esp, 8
mul %ebx
mov %ebx, %eax
L3:
mov %eax, %ebx
jmp L14
L1:
mov %ebx, 1
jmp L3
L14:

add %esp, 0
pop %edi
pop %esi
pop %ebx
pop %ebp
ret

