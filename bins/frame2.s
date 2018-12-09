.intel_syntax
.data

.text

.global _tigermain
.type _tigermain, @function
_tigermain:
push %ebp
mov %ebp, %esp
push %ebx
push %esi
push %edi
sub %esp, 4
L21:
mov %ebx, 0
mov %edi, %ebx
mov %ebx, -16
add %ebx,%ebp
mov [%ebx], %edi
mov %ebx, 0
push %eax
push %ecx
push %edx
mov %eax, 2
push %eax
push %ebp
call L2
add %esp, 8
pop %edx
pop %ecx
pop %eax
jmp L20
L20:

add %esp, 4
pop %edi
pop %esi
pop %ebx
pop %ebp
ret

.global L2
.type L2, @function
L2:
push %ebp
mov %ebp, %esp
push %ebx
push %esi
push %edi
sub %esp, 0
L9:
mov %edi, [%ebp +12]
mov %ebx, [%ebp +8]
mov %ebx, [%ebx -16]
cmp %edi, %ebx
je L3
jmp L4
L4:
mov %ebx, [%ebp +12]
push %eax
push %ecx
push %edx
mov %eax, [%ebp +12]
mov %ecx, 1
sub %eax,%ecx
push %eax
mov %eax, [%ebp +8]
push %eax
call L2
add %esp, 8
pop %edx
pop %ecx
pop %eax
mov %eax, %ebx
mul %eax
mov %ebx, %eax
L5:
mov %eax, %ebx
jmp L8
L3:
mov %ebx, 1
jmp L5
L8:

add %esp, 0
pop %edi
pop %esi
pop %ebx
pop %ebp
ret

