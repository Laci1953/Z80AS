	cseg
a1:	ld	a,1
a2:	ld	a,2
a3:	ld	a,3
	dseg
d1:	ds	10
d2:	ds	10
d3:	ds	10
	dw	d2+d3
	dw	d2+a2
	global	c1
	dw	c1+a1
	dw	c1-a1
	dw	c1+d1
	dw	c1-d1
	dw	a1+d1
	dw	a1-d1
	end
