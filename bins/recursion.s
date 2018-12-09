.intel_syntax
.data

L5:
.long 1
.string "
"

L4:
.long 10
.string "Todavia no"

L3:
.long 1
.string "
"

L2:
.long 4
.string "Hola"

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
L26:
mov %eax, 0
mov %ebx, -16
add %ebx,%ebp
mov [%ebx], %eax
mov %eax, 5
push %eax
push %ebp
call L1
add %esp, 8
jmp L25
L25:

add %esp, 4
pop %edi
pop %esi
pop %ebx
pop %ebp
ret

.global L1
.type L1, @function
L1:
push %ebp
mov %ebp, %esp
push %ebx
push %esi
push %edi
sub %esp, 0
L11:
mov %ebx, [%ebp +12]
mov %eax, [%ebp +8]
mov %eax, [%eax -16]
cmp %ebx, %eax
je L6
jmp L7
L7:
mov %eax, offset L4
push %eax
call print
add %esp, 4
mov %eax, offset L5
push %eax
call print
add %esp, 4
mov %eax, [%ebp +12]
mov %ebx, 1
sub %eax,%ebx
push %eax
mov %eax, [%ebp +8]
push %eax
call L1
add %esp, 8
mov %ebx, 0
L8:
jmp L10
L6:
mov %eax, offset L2
push %eax
call print
add %esp, 4
mov %eax, offset L3
push %eax
call print
add %esp, 4
mov %ebx, 0
jmp L8
L10:

add %esp, 0
pop %edi
pop %esi
pop %ebx
pop %ebp
ret

