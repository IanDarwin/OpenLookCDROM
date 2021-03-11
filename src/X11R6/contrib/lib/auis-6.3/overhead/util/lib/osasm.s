.globl	ftruncate
.globl _cerror

ftruncate:
	movl	$0x0a28, %eax
	lcall	$0x7,$0
	jc	_cerror
	ret
