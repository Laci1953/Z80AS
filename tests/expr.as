; Expression test

;			Result:	   old         new
;				 ZSM 2.8     ZSM 4.0
;				 -------     -------

a1	equ	1+2*3		;   9           7
a2	equ	-2		;  -2          -2
a3	equ	+2		;   2           2
a4	equ	3*-2		;   1          -6
a5	equ	3*+2		;   5           6

	ld	a,(ix+5)	;   5           5
	ld	a,(ix-5)	;  -5          -5

a6	equ	NOT 2		;  -3          -3
a7	equ	1 + NOT 2	;   2+error    -2
a8	equ	1 + NOT 2 + 3	;   5+error    -5
a9	equ	1 + (NOT 2) + 3	; error         1

x	equ	5
y	equ	6
z	equ	7

a10	equ	x + y * z	;  77          47
a11	equ	x * y + z	;  37          37

a12	equ	1 GT 2		;   0           0
a13	equ	1 LT 2		;  -1          -1

	ld	a,(hl)
	ld	a,(ix+(5+1))	; error         6

	ld	a,(5)
a14	equ	(5)

a15	equ	1 SHL 5
a16	equ	8000h SHR 5

a17	equ	(1 SHL 2) OR (1 SHL 3)

	end
