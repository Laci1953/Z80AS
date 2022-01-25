macro test1	a,b,c
aa	defl	5
	db	a,b,c
	endm
macro	test2	a, b, c
	db	a,b,c,aa
	endm
macro	test3	a, b, c ; d
	db	a,b,c;d
	db	a, b, c  ;d
	endm
	test1	1,2,3
	test1	1, 2, 3
	test2	1,2,3
	test2	1, 2, 3
	test3	1,2,3
	test3	1,2,3,4
	test3	1,2,3;4
	test3	1,2,3	;4
	end
