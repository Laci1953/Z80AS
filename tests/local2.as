	$lall

macro	test1
	local	l1
	ifndef	l1
l1	defl	0
	else
l1	defl	l1+1
	endif
	dw	l1
macro	test2
	local	l2
	ifndef	l2
l2	defl	0
	else
l2	defl	l2+1
	endif
	dw	l1,l2
	endm
l1	defl	l1+1
	dw	l1
	endm

	test1

	test2
	test2

	end
