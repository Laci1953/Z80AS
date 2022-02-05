;**********************************************************************;
;                                                                      ;
;   This file is part of ZSM4, a Z80/Z180/Z280 relocatable macro-      ;
;   assembler written in Z80 assembly.                                 ;
;   Copyright (C) 2017-2020, Hector Peraza.                            ;
;   Modified by Ladislau Szilagyi ( dec 2021 - jan 2022 )              ;
;								       ;
;   This program is free software; you can redistribute it and/or      ;
;   modify it under the terms of the GNU General Public License as     ;
;   published by the Free Software Foundation; either version 2 of     ;
;   the License, or (at your option) any later version.                ;
;                                                                      ;
;   This program is distributed in the hope that it will be useful,    ;
;   but WITHOUT ANY WARRANTY; without even the implied warranty of     ;
;   MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the      ;
;   GNU General Public License for more details.                       ;
;                                                                      ;
;   You should have received a copy of the GNU General Public License  ;
;   along with this program; if not, write to the Free Software        ;
;   Foundation, Inc., 675 Mass Ave, Cambridge, MA 02139, USA.          ;
;                                                                      ;
;**********************************************************************;
	TITLE	Z80AS Macro-Assembler
	SUBTTL	Symbol table routines

*include	ZSM.INC

;public
	global	ID,ADDSYM,SYMLUK,FNDOPC,VALID,VALID1,WERROR,ISDIG
;extern
	global	GNC,PTR1,UCASE,IDBUF,IDLEN,MCHECK,ERRFLG
	global	NEWSYM,SYMTBL,SYMADR,IDADR,VAL,CMNPTR
	global	SYMPTR,SYMMOD,OPCODES,OPLEN,EVFLGS
	global	IDTOUP
	global	ID_OR_NUMBER
	global	NUMfb
	global	ISLETR
	global	TempCnt
	global	AdjustLabels,CURSEG,PC

	psect	text

;-----------------------------------------------------------------------
;
;	Each symbol table entry is of varying length.
;
;	The first byte contains the length in the lower 5 bits
;	and flags in the upper 3 bits.
;
; Symbol modes (bit masks)
;
;DSGSYM	equ	080h		; symbol in data segment
;CSGSYM	equ	040h		; symbol in code segment
;BSGSYM	equ	0C0h		; symbol in bss segment
;GBLSYM	equ	020h		; global symbol
;EXTSYM	equ	010h		; external reference (global not in local source)
;UNDEF	equ	001h		; undefined symbol
;
;	This limits the max length of an identifier to 32 bytes.
;
;	Following the flag/length byte is the name which may be from
;	1 to IDMAX bytes in length.
;
;	Following the name are 2 bytes of value (lo,hi) 
;	and 1 byte of address mode
;
;	The table is scanned sequentially and is ended by a 00 byte.
;
;-----------------------------------------------------------------------
;
;	NUMfb - accept also ID as string of decimal digits, 
;	up to 5 digits, followed by 'f' or 'b'
;
;	A = first digit
;
;	if valid string, 
;		returns CARRY=0, symbol length in IDLEN
;	else
;		returns CARRY=1
;
NUMfb:
	push	hl
	push	de
	push	bc		; save regs
	ld	de,IDBUF
	ld	b,5		; only 5 digits accepted
				; all next chars must be also decimal digits
				; ended with 'f' or 'b'
	ld	hl,(PTR1)
	jr	store
loopd:	ld	a,(hl)
	inc	hl
	cp	'f'
	jr	z,valid
	cp	'b'
	jr	z,valid
	call	ISDIG
	jr	c,notvalid
store:	ld	(de),a		; store char
	inc	de
	djnz	loopd
	ld	a,(hl)
	inc	hl
	cp	'f'
	jr	z,valid
	cp	'b'
	jr	nz,notvalid
valid:	
	ld	c,a		;save final char
	ld	a,(hl)		;check next char
	call	UCASE
	call	IsHexa		;if a hexa char, may be a hex number
	jr	nc,notvalid
	cp	'H'		;if h or H, may be a hex number
	jr	z,notvalid
	ld	(PTR1),hl
	ld	a,5
	sub	b		;CARRY=0
	ld	(IDLEN),a
				;check now the reference type
	ld	a,c
	cp	'f'
	jr	nz,back
				;forw ref
	call	CalcValue	;DE=temp symbol value
	call	SearchTempSym
	call	c,AddTempSym
	jr	c,notvalid	;if table full, abandon
	ld	c,(hl)		;suffix
	call	AdjustSymbol	;add suffix to IDBUF, increment IDLEN
	jr	doneref
back:				
				;back ref
	call	CalcValue	;DE=temp symbol value
	call	SearchTempSym
	jr	c,doneref	;back ref not found == not valid
	ld	c,(hl)		;suffix
	dec	c		;-1
	call	AdjustSymbol	;add suffix-1 to IDBUF, increment IDLEN
doneref:
	or	a		;CARRY=0
	pop	bc
	pop	de
	pop	hl
	ret
notvalid:			; string of decimal digits not found, 
	scf			;CARRY=1
	pop	bc		;restore regs
	pop	de
	pop	hl
	ret
;
;	ID_OR_NUMBER - accept also strings of decimal digits, 
;	up to 2 digits, followed by ':'
;
;	returns delimiter char (':') in A, , symbol length in IDLEN
;
ID_OR_NUMBER:
	push	hl
	push	de
	push	bc		; save regs
	ld	de,IDBUF
	ld	b,5		; only 5 digits accepted
	call	GNC		; skip blanks, get char, do NOT convert to uppercase
	push	af		; char on stack
	call	ISDIG		; first char is a decimal digit?
	jr	c,IDN5
				; if yes, all next chars must be also decimal digits
				; ended with ':'
	ld	hl,(PTR1)
	jr	IDN3
IDN2:	ld	a,(hl)
	inc	hl
	cp	':'
	jr	z,IDN4
	call	ISDIG
	jr	c,IDN5
IDN3:	ld	(de),a		; store char
	inc	de
	djnz	IDN2
	ld	a,(hl)
	inc	hl
	cp	':'
	jr	nz,IDN5
IDN4:	dec	hl
	ld	(PTR1),hl
	pop	hl		; drop char from stack
	ld	a,5
	sub	b
	ld	(IDLEN),a
				;now we have IDBUF=n or nn, IDLEN set
	call	CalcValue	;DE=temp symbol value
	call	SearchTempSym
	call	c,AddTempSym
	jr	c,IDN5		;if table full, abandon
	ld	c,(hl)		;suffix
	push	hl
	call	AdjustSymbol	;add suffix to IDBUF, increment IDLEN
	pop	hl
	inc	(hl)		;increment suffix
	ld	a,':'
	pop	bc
	pop	de
	pop	hl
	ret
IDN5:				; string of decimal digits not found, 
				; continue as for a normal ID
	pop	af
	ld	de,IDBUF
	ld	b,IDMAX
	jr	ID1
;
;	ID - Collect ID and place in IDBUF
;	Returns delimiter character in A, symbol length in IDLEN
;
ID:	push	hl
	push	de
	push	bc		; save regs
	ld	de,IDBUF
	ld	b,IDMAX
	call	GNC		; skip blanks, get char, do NOT convert to uppercase
ID1:
	cp	'*'		; accept * as first char (for *include ...)
	jr	z,ID1_
	call	VALID1
	jr	c,LERROR	; error if not a valid starting char
ID1_:
	ld	hl,(PTR1)
	jr	ID3
;
ID2:	ld	a,(hl)
;	call	UCASE		; do NOT convert to uppercase
	call	VALID
	jr	c,ID4
	inc	hl
ID3:	ld	(de),a
	inc	de
	djnz	ID2
ID7:	ld	a,(hl)
;	call	UCASE		; do NOT convert to uppercase
	call	VALID
	jr	c,ID4
	inc	hl
	jr	ID7
;
ID4:	ld	(PTR1),hl
ID6:	cp	';'
	jr	nz,ID9
ID8:	xor	a
ID9:	ld	c,a
	ld	a,IDMAX
	sub	b
	ld	(IDLEN),a	; store symbol length
	ld	a,c
	pop	bc
	pop	de
	pop	hl
	ret
;
LERROR:	ld	a,'L'
	ld	(ERRFLG),a
	jr	ID8
;
VALID:	call	ISDIG
	ret	nc
VALID1:	call	ISLETR
	ret	nc
	cp	'_'
	ret	z
	cp	'$'
	ret	z
	cp	'?'
	ret	z
;	cp	'.'
;	ret	z
	cp	'@'
	ret	z
	scf
	ret
;
ISDIG:	cp	'0'
	ret	c
	cp	'9'+1
	ccf
	ret
;
ISLETR:	cp	'A'		; accept uppercase
	ret	c
	cp	'Z'+1
	jr	c,ISUP
	cp	'a'		; ...and lowercase
	ret	c
	cp	'z'+1
ISUP:	ccf
	ret
;
;	returns CARRY=1 : is not hexa digit
;		      0 : is a hexa digit (A-F)
IsHexa:
	cp	'A'
	ret	c
	cp	'G'
	ccf
	ret
;
;	ADDSYM - Add symbol to symbol table, if not already there
;
;	Upon entry, IDBUF contains the symbol name and IDLEN its length.
;
;	On return, HL and SYMADR point to value low byte in new entry for
;	possible further update, and IDADR is set to the address of the
;	length+flags field.
;
;	Z=1 if symbol already there, Z=0 otherwise.
;
;	Initial flags field is cleared.
;	Initial value is set to (VAL) and mode to (SYMMOD).
;
ADDSYM:	xor	a
	ld	(NEWSYM),a
	ld	de,(SYMTBL)	; no conflict with reg names
	ld	c,3		; value (2 bytes) + address mode (1 byte)
	call	SYMLUK		; lookup symbol
	ld	(SYMADR),hl	; save address for EQU
	ld	(IDADR),de
	ret	nc		; return if already there, else continue below !!!
;
;	SYMENT - Enter a symbol into the symbol table
;	Upon entry IDBUF is assumed to contain the name of the symbol
;	to enter, name length in IDLEN.
;
;	Entry format:	flags/length          (flags = 3 bits, length = 5 bits)
;			n a m e ...           (up to 31 bytes)
;			value                 (2 bytes)
;			address mode          (1 byte)
;
;	On return HL points to value low byte in new entry for possible
;	further update (used by EQU pseudo operator)
;
SYMENT:	ld	bc,IDMAX+4+1	; max entry length + end marker
	call	MCHECK		; check for available memory
	jr	nc,SYMEN1	; jump if enough
WERROR:	ld	a,'W'
	ld	(ERRFLG),a
	ret
;
SYMEN1:	ld	a,(IDLEN)
	or	a
	jr	nz,SYMEN2
	ld	a,'L'
	ld	(ERRFLG),a
	scf
	ret			; else label error
;
SYMEN2:	ld	hl,(SYMPTR)
	ld	(IDADR),hl	; HL = dest ptr
	ld	c,a
	ld	b,0
	ld	(hl),a		; set up length, clear flags
	inc	hl
	ex	de,hl		; DE = dst
	ld	hl,IDBUF	; HL = src
	ldir			; copy name
	ld	hl,(VAL)
	ex	de,hl		; switch registers
	push	hl
	ld	(hl),e		; set value
	inc	hl
	ld	(hl),d
	inc	hl
	ld	a,(SYMMOD)
	ld	(hl),a		; set mode
;	inc	hl
;	ld	de,(CMNPTR)
;	ld	(hl),e
;	inc	hl
;	ld	(hl),d
	inc	hl
	ld	(SYMPTR),hl
	ld	(8000H),hl
	ld	(hl),0		; set up new pointer and new end marker
	pop	hl
	ld	(SYMADR),hl	; save address for EQU
	or	0FFh		; set Z=0
	ld	(NEWSYM),a
	ret
;
;	SYMLUK - Look up symbols in table
;
;	On entry, DE points to table to search and C contains data field
;	length. Symbol name in IDBUF and length in IDLEN.
;
;	On return, CY set means symbol not found.
;	Else DE points to start of table entry and HL to value low byte
;	in table entry.
;
SYMLUK:	ld	a,(IDLEN)
	ld	b,a
	inc	c		; include flags+length field
SYML1:	ld	a,(de)
	or	a
	scf
	ret	z		; return if end of table
	and	1Fh		; get length of symbol
	cp	b
	jr	nz,SYML4
	push	de
	push	bc
	inc	de		; skip length
	ld	hl,IDBUF
SYML2:	ld	a,(de)
	cp	(hl)
	jr	nz,SYML3
	inc	hl
	inc	de
	djnz	SYML2
SYML3:	pop	bc
	pop	de
	jr	z,SYML5		; branch if match
	ld	a,b
SYML4:	add	a,c		; get full entry length
	ld	l,a
	ld	h,0
	add	hl,de
	ex	de,hl
	jr	SYML1		; advance pointer and continue to loop
;
SYML5:	ld	a,(de)
	ld	b,a
	and	0E0h
	ld	(EVFLGS),a	; save flags for main processor
	ld	a,b
	and	1Fh
	ld	l,a
	ld	h,0
	add	hl,de
	inc	hl
	xor	a
	ret			; point to value low and exit
;
;	AdjustLabels - decrement the value of all labels 
;			that belong to the current segment (CURSEG)
;			and whose value is > (PC)
AdjustLabels:
	ld	hl,(SYMTBL)
adjloop:ld	a,(hl)		;(type & len) or EOL
	inc	hl
	or	a		;if EOL
	ret	z		;return
	ld	c,a		;save-it C
	and	1FH		;A=len
	ld	d,0
	ld	e,a
	add	hl,de		;HL=pointer to value
	ld	a,c
	and	0E0H		;check type
	jr	nz,notlabel
				;it's a label
	ld	e,(hl)
	inc	hl
	ld	d,(hl)		;DE=value
	inc	hl
	ld	a,(hl)		;mode
	inc	hl
	and	SEGMASK
	ld	c,a		;C=label's segment
	ld	a,(CURSEG)
	cp	c		;same as current seg?
	jr	nz,adjloop
				;yes
	ex	de,hl		;DE=pointer in symbols table, HL=value,CARRY=0
	push	hl		;value on stack
	ld	bc,(PC)
	scf			;CARRY=1
	sbc	hl,bc		;val >= PC+1 ?
	pop	hl		;HL=value
	ex	de,hl		;DE=value, HL=pointer in symbols table
	jr	c,adjloop
				;yes
	dec	de		;decrement value
	dec	hl
	dec	hl
	ld	(hl),d		;store-it
	dec	hl
	ld	(hl),e
notlabel:
	inc	hl
	inc	hl
	inc	hl
	jr	adjloop
;
;	Search opcode table using a binary search
;
FNDOPC:				
	call	IDTOUP		;first convert string to uppercase
	ld	a,(IDLEN)
	sub	2
	ret	c
	cp	8+1-2
	ccf
	ret	c
	ld	c,a
	ld	b,0
	ld	hl,OPLEN
	add	hl,bc
	ex	de,hl
	ld	hl,OPCODES
	add	hl,bc
	add	hl,bc
	add	a,4+2
	ld	c,a		; save item size
	ld	a,(hl)
	inc	hl
	ld	h,(hl)
	ld	l,a		; get table base address into HL
	ld	a,(de)
	ld	b,a		; get number of table items into B
	ld	d,c		; get item size into D

	; binary search

	ld	e,0FFh		; mark M <> old M
	ld	c,0		; lower bound = 0
BNEXT:	ld	a,b
	add	a,c
	rra			; middle point = (upper+lower)/2
	cp	e		; same as last time?
	scf
	ret	z		; return error - not found

	ld	e,a		; new middle value
	push	hl		; table base addr
	push	de
	push	bc
	ld	b,d		; get item size into B and C
	ld	c,b		;  for loop counting below

	ld	d,0		; DE = middle point
BADD:	add	hl,de		; index into table (HL += DE times B)
	djnz	BADD		; count and loop

	push	hl
	inc	hl		; address of byte to compare
	ld	de,IDBUF
	ld	a,c
	sub	4
	ld	b,a		; B = string length
BCMP:	ld	a,(de)
	cp	(hl)
	inc	de
	inc	hl
	jr	nz,DIFF		; branch if no match
	djnz	BCMP
	pop	hl		; match found
	pop	bc
	pop	de
	pop	bc
	ld	a,e		; return index in A
	ex	de,hl
	jp	SYML5

DIFF:	pop	hl
	pop	bc		; restore bounds
	pop	de		; restore item size and middle point
	pop	hl		; restore table base address
	jr	c,LOWER		; IDBUF is lower
	ld	c,e		; lower = middle
	jr	BNEXT		; IDBUF is higher

LOWER:	ld	b,e		; upper = middle
	jr	BNEXT		; IDBUF is lower
;
;	Convert ID to uppercase
;
IDTOUP:	ld	hl,IDBUF
	ld	a,(IDLEN)
	or	a
	ret	z
	ld	b,a
TOUP:	ld	a,(hl)
	call	UCASE
	ld	(hl),a
	inc	hl
	djnz	TOUP	
	ret
;
;	Search a temporary symbol
;	DE = symbol value to be found
;	returns CARRY = 0 : HL = symbol sufix pointer
;		CARRY = 1 : not found, HL=pointer to EOL
;	HL,B affected
;
SearchTempSym:
	ld	a,(TempCnt)
	or	a		;empty?
	scf
	ret	z		;yes, return CARRY=1
	ld	hl,TempSym
	ld	b,a		;B=counter
sloop:	ld	a,(hl)		;low
	inc	hl
	cp	e
	jr	nz,nexts
	ld	a,(hl)		;high
	inc	hl
	cp	d
	ret	z		;CARRY=0
	jr	nextss		
nexts:	inc	hl
nextss:	inc	hl
	djnz	sloop	
	scf			;no one matched
	ret
;
;	Add a temporary symbol
;
;	DE = symbol value to add
;	returns CARRY=0, HL=pointer to suffix
;		CARRY=1 table full
;	HL,BC affected
;
AddTempSym:
	ld	a,(TempCnt)
	ld	c,a
	inc	a
	jr	z,full
	ld	(TempCnt),a
	ld	a,c
	add	a,a		;*2
	add	a,c		;*3
	ld	c,a
	ld	b,0
	ld	hl,TempSym
	add	hl,bc		;CARRY=0
	ld	(hl),e		;store val 
	inc	hl
	ld	(hl),d
	inc	hl
	ld	a,'A'		;set initial sufix
	ld	(hl),a
	ret
full:	scf
	ret
;
;	Calculate temporary symbol value
;	symbol in IDBUF, length in IDLEN
;	returns DE = value
;	HL,BC affected
CalcValue:
	ld	hl,IDBUF
	ld	a,(IDLEN)
	ld	b,a
	ld	de,0		;init DE=val=0
nextchar:
	ld	a,(hl)		;get next char
	sub	'0'
	ld	c,a		;save char val
	push	hl
	ex	de,hl		;HL=val
	add	hl,hl		;*2
	ld	e,l
	ld	d,h
	add	hl,hl
	add	hl,hl
	add	hl,hl		;val=val*10
	ld	e,c
	ld	d,0
	add	hl,de
	ex	de,hl		;DE=val
	pop	hl
	inc	hl
	djnz	nextchar
	ret			;DE=val
;
;	Adjust symbol
;
;	C=suffix
;	DE,HL affected
;
AdjustSymbol:
	ld	a,(IDLEN)
	ld	e,a
	ld	d,0
	ld	hl,IDBUF
	add	hl,de
	ld	(hl),c		;store suffix
	inc	a		
	ld	(IDLEN),a	;save incremented IDLEN
	ret
;
	psect	data
;
TempCnt:defs	2	;counter
TempSym:defs	300H
;
;	value (word)
;	sufix (byte)
;
