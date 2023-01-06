global main

section .text

main:
	push rbp
	mov rbp, rsp
	mov rax, 3
	mov rbx, 1
	push 1
	push 2
	push 3
	push 4
	mov qword [rbp + (4 + 2) * 8], rax
	mov rbx, [rsp + (rbx + 2) * 8]
	mov rax, 0
	leave
	
