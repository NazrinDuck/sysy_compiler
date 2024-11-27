	.text
	.globl main
main:
	li	t1, 9
	li	t2, 10
	sgt	t0, t1, t2
	seqz	t0, t0
	mv	a0, t0
	ret
