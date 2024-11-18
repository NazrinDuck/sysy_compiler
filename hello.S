	.text
	.globl main
main:
	li	t1, 2
	sub	t0, x0, t1
	li	t2, 1
	xor	t1, t2, x0
	seqz	t1, t1
	add	t2, t0, t1
	li	t1, 1
	li	t3, 2
	sub	t0, t1, t3
	mul	t1, t2, t0
	mv	a0, t1
	ret
