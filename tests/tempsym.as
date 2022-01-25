
	jr	1f
1:	nop
	jr	nc,1b
	jr	16384f
16384:	nop
	jr	16384f
	jr	z,1b
16384:	nop
22:	nop
	jr	22b
22:	nop
	jr	c,22f
	jr	nz,1f
1:	nop
22:	nop
	jp	22b
