.intel_syntax
.data

L5:
.long 4
.string "hola"

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
L11:
mov %ebx, 0
mov %ebx, 0
mov %edi, %ebx
mov %ebx, 2
cmp %edi, %ebx
jle L8
jmp L2
L2:
jmp L10
L8:
mov %eax, 0
mov %ebx, %eax
mov %eax, 2
cmp %ebx, %eax
jle L6
jmp L4
L4:
mov %eax, 2
cmp %edi, %eax
je L2
jmp L9
L9:
mov %eax, 1
add %eax,%edi
mov %edi, %eax
jmp L8
L6:
mov %eax, offset L5
push %eax
call print
add %esp, 4
mov %edi, 2
cmp %ebx, %edi
je L4
jmp L7
L7:
mov %edi, 1
add %edi,%ebx
mov %ebx, %edi
jmp L6
L10:

add %esp, 0
pop %edi
pop %esi
pop %ebx
pop %ebp
ret

