macro	test1	a,b,c
	db	a,b,c
	endm
macro	test2
	test1	1,2,3
	endm
	test1	1,2,3
	test2
	end
