.intel_syntax
.data

L29:
.long 1
.string "
"

L26:
.long 1
.string "
"

L19:
.long 2
.string " ."

L18:
.long 2
.string " O"

.text

.global _tigermain
.type _tigermain, @function
_tigermain:
push %ebp
mov %ebp, %esp
push %ebx
push %esi
push %edi
sub %esp, 20
L210:
mov %eax, 8
mov %ebx, -16
add %ebx,%ebp
mov [%ebx], %eax
mov %eax, 0
push %eax
mov %eax, [%ebp -16]
push %eax
call _initArray
add %esp, 8
mov %ebx, -20
add %ebx,%ebp
mov [%ebx], %eax
mov %eax, 0
push %eax
mov %eax, [%ebp -16]
push %eax
call _initArray
add %esp, 8
mov %ebx, -24
add %ebx,%ebp
mov [%ebx], %eax
mov %eax, 0
push %eax
mov %ebx, [%ebp -16]
mov %eax, [%ebp -16]
add %eax,%ebx
mov %ebx, 1
sub %eax,%ebx
push %eax
call _initArray
add %esp, 8
mov %ebx, -28
add %ebx,%ebp
mov [%ebx], %eax
mov %eax, 0
push %eax
mov %ebx, [%ebp -16]
mov %eax, [%ebp -16]
add %eax,%ebx
mov %ebx, 1
sub %eax,%ebx
push %eax
call _initArray
add %esp, 8
mov %ebx, -32
add %ebx,%ebp
mov [%ebx], %eax
mov %eax, 0
push %eax
push %ebp
call L11
add %esp, 8
jmp L209
L209:

add %esp, 20
pop %edi
pop %esi
pop %ebx
pop %ebp
ret

.global L11
.type L11, @function
L11:
push %ebp
mov %ebp, %esp
push %ebx
push %esi
push %edi
sub %esp, 4
L108:
mov %edi, [%ebp +12]
mov %ebx, [%ebp +8]
mov %ebx, [%ebx -16]
cmp %edi, %ebx
je L70
jmp L71
L71:
mov %ebx, 0
mov [%ebp -16],%ebx
mov %ebx, [%ebp +8]
mov %ebx, [%ebx -16]
mov %esi, 1
mov %edi,%ebx
sub %edi,%esi
mov %ebx, [%ebp -16]
cmp %ebx, %edi
jle L68
jmp L31
L31:
mov %ebx, 0
L72:
jmp L107
L70:
mov %eax, [%ebp +8]
push %eax
call L10
add %esp, 4
mov %eax, 0
mov %ebx, %eax
jmp L72
L68:
mov %eax, [%ebp +8]
mov %eax, [%eax -20]
mov %edi, %eax
mov %eax, [%ebp -16]
mov %ebx, %eax
push %ebx
push %edi
call _checkIndexArray
add %esp, 8
mov %ecx, 4
mov %eax, %ebx
mul %ecx
add %eax,%edi
mov %ebx, [%eax]
mov %eax, 0
cmp %ebx, %eax
je L39
jmp L40
L40:
mov %eax, 0
L41:
mov %ebx, 0
cmp %eax, %ebx
jne L48
jmp L49
L49:
mov %eax, 0
L50:
mov %ebx, 0
cmp %eax, %ebx
jne L66
jmp L67
L67:
mov %eax, [%ebp +8]
mov %eax, [%eax -16]
mov %ebx, 1
sub %eax,%ebx
mov %ebx, [%ebp -16]
cmp %ebx, %eax
je L31
jmp L69
L69:
mov %eax, 1
mov %ebx, [%ebp -16]
add %eax,%ebx
mov [%ebp -16],%eax
jmp L68
L39:
mov %eax, 1
mov %ebx, %eax
mov %eax, [%ebp +8]
mov %eax, [%eax -28]
mov %esi, %eax
mov %eax, [%ebp +12]
mov %ecx, [%ebp -16]
add %eax,%ecx
mov %edi, %eax
push %edi
push %esi
call _checkIndexArray
add %esp, 8
mov %ecx, 4
mov %eax, %edi
mul %ecx
add %eax,%esi
mov %ecx, [%eax]
mov %eax, 0
cmp %ecx, %eax
je L37
jmp L38
L38:
mov %eax, 0
mov %ebx, %eax
L37:
mov %eax, %ebx
jmp L41
L48:
mov %eax, 1
mov %ebx, %eax
mov %eax, [%ebp +8]
mov %eax, [%eax -32]
mov %esi, %eax
mov %eax, 7
mov %ecx, [%ebp -16]
add %eax,%ecx
mov %ecx, [%ebp +12]
sub %eax,%ecx
mov %edi, %eax
push %edi
push %esi
call _checkIndexArray
add %esp, 8
mov %ecx, 4
mov %eax, %edi
mul %ecx
add %eax,%esi
mov %ecx, [%eax]
mov %eax, 0
cmp %ecx, %eax
je L46
jmp L47
L47:
mov %eax, 0
mov %ebx, %eax
L46:
mov %eax, %ebx
jmp L50
L66:
mov %eax, [%ebp +8]
mov %eax, [%eax -20]
mov %edi, %eax
mov %eax, [%ebp -16]
mov %ebx, %eax
push %ebx
push %edi
call _checkIndexArray
add %esp, 8
mov %ecx, 4
mov %eax, %ebx
mul %ecx
add %eax,%edi
mov %ebx, 1
mov [%eax], %ebx
mov %eax, [%ebp +8]
mov %eax, [%eax -28]
mov %edi, %eax
mov %eax, [%ebp +12]
mov %ebx, [%ebp -16]
add %eax,%ebx
mov %ebx, %eax
push %ebx
push %edi
call _checkIndexArray
add %esp, 8
mov %ecx, 4
mov %eax, %ebx
mul %ecx
add %eax,%edi
mov %ebx, 1
mov [%eax], %ebx
mov %eax, [%ebp +8]
mov %eax, [%eax -32]
mov %edi, %eax
mov %eax, 7
mov %ebx, [%ebp -16]
add %eax,%ebx
mov %ebx, [%ebp +12]
sub %eax,%ebx
mov %ebx, %eax
push %ebx
push %edi
call _checkIndexArray
add %esp, 8
mov %ecx, 4
mov %eax, %ebx
mul %ecx
add %eax,%edi
mov %ebx, 1
mov [%eax], %ebx
mov %eax, [%ebp +8]
mov %eax, [%eax -24]
mov %edi, %eax
mov %eax, [%ebp +12]
mov %ebx, %eax
push %ebx
push %edi
call _checkIndexArray
add %esp, 8
mov %ecx, 4
mov %eax, %ebx
mul %ecx
add %eax,%edi
mov %ebx, [%ebp -16]
mov [%eax], %ebx
mov %ebx, [%ebp +12]
mov %eax, 1
add %eax,%ebx
push %eax
mov %eax, [%ebp +8]
push %eax
call L11
add %esp, 8
mov %eax, [%ebp +8]
mov %eax, [%eax -20]
mov %edi, %eax
mov %eax, [%ebp -16]
mov %ebx, %eax
push %ebx
push %edi
call _checkIndexArray
add %esp, 8
mov %ecx, 4
mov %eax, %ebx
mul %ecx
add %eax,%edi
mov %ebx, 0
mov [%eax], %ebx
mov %eax, [%ebp +8]
mov %eax, [%eax -28]
mov %edi, %eax
mov %eax, [%ebp +12]
mov %ebx, [%ebp -16]
add %eax,%ebx
mov %ebx, %eax
push %ebx
push %edi
call _checkIndexArray
add %esp, 8
mov %ecx, 4
mov %eax, %ebx
mul %ecx
add %eax,%edi
mov %ebx, 0
mov [%eax], %ebx
mov %eax, [%ebp +8]
mov %eax, [%eax -32]
mov %esi, %eax
mov %eax, 7
mov %ebx, [%ebp -16]
add %eax,%ebx
mov %ebx, [%ebp +12]
sub %eax,%ebx
mov %edi, %eax
push %edi
push %esi
call _checkIndexArray
add %esp, 8
mov %ebx, 4
mov %eax, %edi
mul %ebx
mov %edi,%eax
add %edi,%esi
mov %ebx, 0
mov [%edi], %ebx
jmp L67
L107:

add %esp, 4
pop %edi
pop %esi
pop %ebx
pop %ebp
ret

.global L10
.type L10, @function
L10:
push %ebp
mov %ebp, %esp
push %ebx
push %esi
push %edi
sub %esp, 4
L75:
mov %eax, 0
mov %ebx, %eax
mov %eax, [%ebp +8]
mov %eax, [%eax -16]
mov %ecx, 1
sub %eax,%ecx
cmp %ebx, %eax
jle L27
jmp L13
L13:
mov %eax, offset L29
push %eax
call print
add %esp, 4
jmp L74
L27:
mov %eax, 0
mov [%ebp -16],%eax
mov %eax, [%ebp +8]
mov %eax, [%eax -16]
mov %ecx, 1
sub %eax,%ecx
mov %ecx, [%ebp -16]
cmp %ecx, %eax
jle L24
jmp L15
L15:
mov %eax, offset L26
push %eax
call print
add %esp, 4
mov %eax, [%ebp +8]
mov %eax, [%eax -16]
mov %ecx, 1
sub %eax,%ecx
cmp %ebx, %eax
je L13
jmp L28
L28:
mov %eax, 1
add %eax,%ebx
mov %ebx, %eax
jmp L27
L24:
mov %eax, [%ebp +8]
mov %eax, [%eax -24]
mov %esi, %eax
mov %edi, %ebx
push %edi
push %esi
call _checkIndexArray
add %esp, 8
mov %ebx, 4
mov %eax, %edi
mul %ebx
mov %ebx,%eax
add %ebx,%esi
mov %ebx, [%ebx]
mov %edi, [%ebp -16]
cmp %ebx, %edi
je L20
jmp L21
L21:
mov %eax, offset L19
mov %ebx, %eax
L22:
push %ebx
call print
add %esp, 4
mov %ebx, [%ebp +8]
mov %ebx, [%ebx -16]
mov %esi, 1
mov %edi,%ebx
sub %edi,%esi
mov %ebx, [%ebp -16]
cmp %ebx, %edi
je L15
jmp L25
L25:
mov %ebx, 1
mov %edi,%ebx
mov %ebx, [%ebp -16]
add %edi,%ebx
mov %ebx, %edi
mov [%ebp -16],%ebx
jmp L24
L20:
mov %ebx, offset L18
jmp L22
L74:

add %esp, 4
pop %edi
pop %esi
pop %ebx
pop %ebp
ret

