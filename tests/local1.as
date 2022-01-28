	$lall

	irpc	c,abc
macro	m&c	
	local	d		;; allowed
d	aset	d+1
	db	'&c'
	endm
	endm

	ma
	mb
	mc

	end
