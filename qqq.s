.intel_syntax
.data

L3:
.long 1
.string "A"

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
L20:
push %ebp
call L0_g
add %esp, 4
jmp L19
L19:

add %esp, 0
pop %edi
pop %esi
pop %ebx
pop %ebp
ret

.global L0_g
.type L0_g, @function
L0_g:
push %ebp
mov %ebp, %esp
push %ebx
push %esi
push %edi
sub %esp, 4
L14:
mov %eax, 10
mov %ebx, -16
add %ebx,%ebp
mov [%ebx], %eax
push %ebp
call L2_f
add %esp, 4
mov %eax, 0
jmp L13
L13:

add %esp, 4
pop %edi
pop %esi
pop %ebx
pop %ebp
ret

.global L2_f
.type L2_f, @function
L2_f:
push %ebp
mov %ebp, %esp
push %ebx
push %esi
push %edi
sub %esp, 0
L8:
mov %eax, [%ebp +8]
mov %eax, [%eax -16]
mov %ebx, %eax
mov %eax, offset L3
push %eax
call ord
add %esp, 4
add %eax,%ebx
push %eax
call chr
add %esp, 4
push %eax
call print
add %esp, 4
jmp L7
L7:

add %esp, 0
pop %edi
pop %esi
pop %ebx
pop %ebp
ret

