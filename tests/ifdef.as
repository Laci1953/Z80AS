; IFDEF/IFNDEF conditional test

abc	equ	1

	global	def

	ifdef	abc
	db	1	; yes
	endif

	ifdef	def
	db	1	; no
	endif

	ifdef	ghi
	db	1	; no
	endif

	ifndef	abc
	db	1	; no
	endif

	ifndef	def
	db	1	; yes
	endif

	ifndef	ghi
	db	1	; yes
	endif

	ifdef	abc
	db	1	; yes
	else
	db	1	; no
	endif

	ifdef	def
	db	1	; no
	else
	db	1	; yes
	endif

	ifdef	ghi
	db	1	; no
	else
	db	1	; yes
	endif

	ifndef	abc
	db	1	; no
	else
	db	1	; yes
	endif

	ifndef	def
	db	1	; yes
	else
	db	1	; no
	endif

	ifndef	ghi
	db	1	; yes
	else
	db	1	; no
	endif

	end
