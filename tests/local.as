; LOCAL variables in MACROs

	cseg

macro	ltest	a,b,c
	local	x,y,z
x::	db	a
y::	db	b
	dw	x
	endm

macro	ltest2	a,b,c
	local	x
	local	y
	local	z
x:	db	a
y:	db	b
	dw	x
	endm

	ltest	1,2,3
	ltest	4,5,6
	ltest	7,8,9

	ltest2	10,11,12
	ltest2	20,21,22

	end
