global main

section .text

main:
	push rbp
	mov rbp, rsp
	mov rax, 3
	mov rbx, 1
	mov rbx, [rsp + (rbx + 2) * 8]
	mov rax, 0
	leave
	
