; IFIDN/IFDIF conditional test

	list	nocond,macros

val	equ	1234

macro	streq	s1,s2
	ifidn	<s1>,<s2>
	$printx	"<&s1> and <&s2> are identical"
	else
	$printx	"<&s1> and <&s2> are different"
	endif
	ifdif	<s1>,<s2>
	$printx	"<&s1> and <&s2> are different"
	else
	$printx	"<&s1> and <&s2> are identical"
	endif
	endm

	streq	1,2
	streq	,a
	streq	a,
	streq	123456,123456
	streq	,aa		; different
	streq	aa,		; different
	streq	123456,123456	; identical
	streq	,
	streq	,		; identical
	streq	'abc','abc'
	streq	%val,%val

	end

