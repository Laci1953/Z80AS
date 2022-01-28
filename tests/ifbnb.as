; IFB/IFNB conditional test

	cseg

macro	isblank str
	ifb	<&str>
	$printx	'<&str> is blank'
	else
	$printx	'<&str> is not blank'
	endif
	ifnb	<&str>
	$printx	'<&str> is not blank'
	else
	$printx	'<&str> is blank'
	endif
	endm

	ifb	<>
	db	1
	endif

	ifnb	<>
	db	1
	endif

	ifb	<a>
	db	1
	endif

	ifnb	<a>
	db	1
	endif

	isblank
	isblank			; is blank
	isblank	a
	isblank	a		; is not blank
	isblank	<a,b,c>

	end
