
	if	1
	db	1
	else
	db	2
	else		; 'C' error
	db	3
	endif

	if	0
	db	1
	else
	db	2
	else		; 'C' error
	db	3
	endif

	if	0
	if	1
	db	1
	else
	db	2
	else		; no error
	db	3
	endif
	endif

	else		; 'C' error

	if	1

	end		; 'T' error
