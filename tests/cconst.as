; Character constant test

	db	1,'abc',2,'a'+1,5
	db	'ab'

	db	'a'		; 61
	db	'a'+1		; 62

	dw	'ab'		; 61 62
	dw	'ab'+1		; 61 63

	dw	'a'		; 61 00
	dw	'a'+1		; 62 00

	db	''''		; 27
	db	''''+1		; 28

	dw	'''a'		; 27 61
	dw	'a'''		; 61 27

	dw	'''a'+1		; 27 62
	dw	'a'''+1		; 61 28

	ld	a,''''		; 27
	ld	a,''''+1	; 28

	ld	hl,'''a'	; 27 61
	ld	hl,'''a'+1	; 27 62
	ld	hl,'a'''	; 61 27
	ld	hl,'a'''+1	; 61 28

	db	''
	ld	a,''
	ld	a,''		; 3E 00
	ld	a,''''

	ld	hl,''''''	; 21 27 27
	ld	hl,''''''

	db	'a'
	db	'a'+1

	dw	'ab'
	dw	'ab'+1

	end
