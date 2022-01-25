macro	test1	a,b,c
	db	a,b,c
	endm

macro	test2	a,b
macro	test3 c
	db	c
	endm
	test3	a
	db	b
	endm

	test1	1,2,3
	test2	4,5
	test3	6
	test2	7,8

	end
