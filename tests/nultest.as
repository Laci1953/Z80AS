	cseg

	if	1
	list	nocond,macros
	else
	$lfcond
	$lall
	endif

macro	test	arg1,arg2
	if2
	if	NUL arg1
	$printx	'arg1 is null'
	endif
	if	NUL arg2
	$printx	'arg2 is null'
	endif
	endif
	endm

macro	test2	arg1,arg2
	if2
	$printx	'arg1=&arg1, arg2=&arg2'
	if	arg1 AND NOT NUL arg2
	$printx	'arg1<>0 AND arg2 is NOT null'
	endif
	$printx	''
	endif
	endm

	test
	test	,
	test	1
	test	1,2
	test	,2
	test	1,
	test	<>,2
	test	1,<>

	test			; comment
	test	,		; comment
	test	1		; comment
	test	1,2		; comment
	test	,2		; comment
	test	1,		; comment
	test	<>,2		; comment
	test	1,<>		; comment

	test2	0
	test2	1
	test2	0,2
	test2	1,2
	test2	0,<>
	test2	1,<>
	test2	0,
	test2	1,

	test2			; error

	end
