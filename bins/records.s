.intel_syntax
.data

L31:
.long 3
.string "sdf"

L29:
.long 4
.string "kati"

L20:
.long 5
.string "Allos"

L17:
.long 5
.string "Kapou"

L16:
.long 7
.string "Kapoios"

L13:
.long 0
.string ""

L9:
.long 9
.string "somewhere"

L8:
.long 5
.string "aname"

.text

.global _tigermain
.type _tigermain, @function
_tigermain:
push %ebp
mov %ebp, %esp
push %ebx
push %esi
push %edi
sub %esp, 12
L38:
push %eax
push %ecx
push %edx
mov %eax, 0
push %eax
mov %eax, 10
push %eax
call _initArray
add %esp, 8
pop %edx
pop %ecx
pop %eax
mov %esi, %eax
push %eax
push %ecx
push %edx
mov %eax, 0
push %eax
mov %eax, 0
push %eax
mov %eax, offset L9
push %eax
mov %eax, offset L8
push %eax
mov %eax, 4
push %eax
call _allocRecord
add %esp, 20
pop %edx
pop %ecx
pop %eax
push %eax
push %ecx
push %edx
push %eax
mov %eax, 5
push %eax
call _initArray
add %esp, 8
pop %edx
pop %ecx
pop %eax
mov %ebx, %eax
mov [%ebp -16],%ebx
push %eax
push %ecx
push %edx
mov %eax, offset L13
push %eax
mov %eax, 100
push %eax
call _initArray
add %esp, 8
pop %edx
pop %ecx
pop %eax
mov %ebx, %eax
push %eax
push %ecx
push %edx
mov %eax, 44
push %eax
mov %eax, 2432
push %eax
mov %eax, offset L17
push %eax
mov %eax, offset L16
push %eax
mov %eax, 4
push %eax
call _allocRecord
add %esp, 20
pop %edx
pop %ecx
pop %eax
mov %ebx, %eax
mov [%ebp -24],%ebx
push %eax
push %ecx
push %edx
mov %eax, 1900
push %eax
mov %eax, 3
push %eax
call _initArray
add %esp, 8
pop %edx
pop %ecx
pop %eax
push %eax
push %ecx
push %edx
push %eax
mov %eax, offset L20
push %eax
mov %eax, 2
push %eax
call _allocRecord
add %esp, 12
pop %edx
pop %ecx
pop %eax
mov %ebx, %eax
mov [%ebp -20],%ebx
mov %edi, %esi
mov %ebx, 0
push %eax
push %ecx
push %edx
push %ebx
push %edi
call _checkIndexArray
add %esp, 8
pop %edx
pop %ecx
pop %eax
mov %edx, 4
mov %eax, %ebx
mul %edx
mov %ebx,%eax
add %ebx,%edi
mov %edi, 1
mov [%ebx], %edi
mov %edi, %esi
mov %ebx, 9
push %eax
push %ecx
push %edx
push %ebx
push %edi
call _checkIndexArray
add %esp, 8
pop %edx
pop %ecx
pop %eax
mov %edx, 4
mov %eax, %ebx
mul %edx
mov %ebx,%eax
add %ebx,%edi
mov %edi, 3
mov [%ebx], %edi
mov %ebx, offset L29
mov %esi, %ebx
mov %ebx, [%ebp -16]
mov %edi, %ebx
mov %ebx, 3
push %eax
push %ecx
push %edx
push %ebx
push %edi
call _checkIndexArray
add %esp, 8
pop %edx
pop %ecx
pop %eax
mov %edx, 4
mov %eax, %ebx
mul %edx
mov %ebx,%eax
add %ebx,%edi
mov %edi, [%ebx]
mov %ebx, 0
add %ebx,%edi
mov [%ebx], %esi
mov %ebx, offset L31
mov %edi, %ebx
mov %ebx, 0
mov %esi, [%ebp -24]
add %ebx,%esi
mov [%ebx], %edi
mov %ebx, [%ebp -20]
mov %ebx, [%ebx +4]
mov %edi, %ebx
mov %ebx, 0
push %eax
push %ecx
push %edx
push %ebx
push %edi
call _checkIndexArray
add %esp, 8
pop %edx
pop %ecx
pop %eax
mov %edx, 4
mov %eax, %ebx
mul %edx
mov %ebx,%eax
add %ebx,%edi
mov %edi, 2323
mov [%ebx], %edi
mov %ebx, [%ebp -20]
mov %ebx, [%ebx +4]
mov %edi, %ebx
mov %ebx, 2
push %eax
push %ecx
push %edx
push %ebx
push %edi
call _checkIndexArray
add %esp, 8
pop %edx
pop %ecx
pop %eax
mov %edx, 4
mov %eax, %ebx
mul %edx
mov %ebx,%eax
add %ebx,%edi
mov %edi, 2323
mov [%ebx], %edi
push %eax
push %ecx
push %edx
mov %eax, [%ebp -24]
mov %eax, [%eax +0]
push %eax
call print
add %esp, 4
pop %edx
pop %ecx
pop %eax
jmp L37
L37:

add %esp, 12
pop %edi
pop %esi
pop %ebx
pop %ebp
ret

