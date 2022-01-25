	cseg

a1	equ	1234h			; 1234

b1	equ	LOW a1			; 0034
b2	equ	HIGH a1			; 0012

c1	equ	LOW(a1)			; 0034
c2	equ	LOW(a1)+1000h		; 1034
c3	equ	LOW(a1+1000h)		; 0034

a2	equ	LOW a1 + 1000h		; 1034
a3	equ	HIGH a1 + 1000h		; 1012
a4	equ	1000h + LOW a1		; 1034
a5	equ	1000h + HIGH a1		; 1012

a8	equ	NOT a1			; EDCB

a10	equ	LOW a1 OR HIGH a1 SHL 8	; 1234

a11	equ	1 SHL 1 + 1		; 0003

	end
