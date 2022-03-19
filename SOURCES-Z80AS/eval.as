;**********************************************************************;
;                                                                      ;
;   This file is part of ZSM4, a Z80/Z180/Z280 relocatable macro-      ;
;   assembler written in Z80 assembly.                                 ;
;   Copyright (C) 2017-2020, Hector Peraza.                            ;
;   Modified by Ladislau Szilagyi ( dec 2021 - jan 2022 )              ;
;                                                                      ;
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
	SUBTTL	Expression Evaluation

*include	ZSM.INC

	psect	text
;public
	global	EVAL,EVALNEW,EVALREG,EVALSRG,EVALCND,EVBRKT
	global	REGVAL,CCONST,SYMPT,RELERR,INT,INTBUF
;extern
	global	SYMTBL,IDBUF,IDLEN,VALID1,OERROR,ADDEXT
	global	ERRFLG,PCFLAG,EVFLGS,PC,CURSEG	;,CURCMN
	global	CPU,SYMADR,BACKUP,GNC,UFLAG,PTR1,VAL,ID
	global	SYMLUK,UCASE,LEN,DBWFLG,EVMODE,EXTCHN
	global	RADIX,GETPPC,CMNPTR,CMPHD,CHK8U
	global	VALERR,REQCHR,UMODE,SYMMOD,ADDSYM
	global	IDTOUP
	global	NUMfb
	global	ISLETR
	global	LASTEXTSYM
	global	OPMODE
	global	JFLAG,DSFLAG
;
;	Evaluate expression in () or <> brackets, assumes the opening
;	bracket has been already scanned, and is passed to the routine
;	in reg A.
;
;	Returns a code in regs A and C indicating the type of operand:
;
;	Code	Operand		Notes
;	----	-------		-----
;	 0	(addr)		address stored in variable VAL,
;				mode in EVMODE
;	 1	(r)		r can only be reg C
;	 2	(rp)		rp can be BC, DE, HL or SP
;				register value stored in REGVAL
;	 3	(rp+d8)		rp is either IX or IY
;				displacement stored in VAL,
;				mode in EVMODE
;				* in Z80 and Z180 modes a 'V' error is
;				  automatically generated if displacement
;				  is not a signed 8-bit value.
;				* in Z280 mode code 4 is returned if
;				  displacement is a 16-bit quantity.
;	 4	(rp+d16)	Z280 mode only, rp can be HL, IX, IY or SP
;				displacement stored in VAL,
;				mode stored in EVMODE
;	 5	(x+y)		Z280 mode only, returns B = 0 for (HL+IX),
;				1 for (HL+IY) or 2 for (IX+IY)
;	 6	(PC+d16)	Z280 mode only
;				displacement stored in VAL,
;				mode in EVMODE
;	 7	<addr>		Z280 mode only
;				address stored in VAL, mode in EVMODE
;
EVBRKT:	cp	'<'
	jr	z,EVREL
	call	EVALSRG		; include special regs in search
	ld	(REGVAL),hl
	cp	RPNAME
	jr	z,EVRP		; branch if rpair
	cp	RNAME
	jr	z,EVSG		; branch if single reg
	cp	RSNAME
	jr	z,EVRS		; branch if special reg (Z280)
	ld	c,0		; code 0 = (addr), value already in VAL
EVRET:	call	REQCHR
	defb	')'
	ld	a,c
	ret
;
EVREL:	ld	c,7		; code 7 = <addr>
	ld	a,(CPU)
	cp	2
	jr	nz,EVERR	; Z280 only
	push	bc
	call	EVALNEW		; get address
	pop	bc
	call	REQCHR
	defb	'>'
	ld	a,c
	ret
;
EVERR:	call	OERROR		; set error flag
	ld	a,c
	ret
;
EVSG:	ld	a,l
	dec	a
	ld	c,1		; code 1 = (C)
	jr	nz,EVERR	; single register can only be C
	jr	EVRET
;
EVRS:	ld	bc,EVRET
	push	bc		; push return address
	ld	a,l
	cp	3
	jr	nz,EVERR	; special register can only be PC
	ld	c,6		; code 6 = (PC+d16)
	ld	a,(CPU)
	cp	2
	jr	nz,EVERR	; Z280 only
	push	bc
	call	EVALNEW		; get index
	pop	bc
	ret
;
EVRP:	ld	bc,EVRET
	push	bc		; push return address
	ld	a,l
	cp	4
	jr	nz,EVRP2	; branch if not HL/IX/IY
	ld	a,(CPU)
	cp	2		; Z280 mode?
	jr	nz,EVRP2	; skip if not
	ld	hl,(PTR1)	; else check for X+Y modes
	push	hl		; save record pointer
	call	GNC
	cp	'+'
	jr	nz,EVRP1	; branch if (HL), (IX) or (IY)
	call	EVALREG		; else get second register
	cp	RPNAME
	jr	nz,EVRP1	; branch if not (x+y)
	ld	a,l
	cp	4		; must be HL, IX or IY
	jr	nz,EVRP1
	ld	c,5		; code 5 = (x+y)
	ld	a,(REGVAL+1)
	xor	h
	pop	hl
	cp	0DDh		; HL+IX
	ld	b,1
	ret	z
	inc	b
	cp	0FDh		; HL+IY
	ret	z
	inc	b		; else is IX+IY
	ret
;
EVRP1:	pop	hl
	ld	(PTR1),hl	; restore record pointer
EVRP2:	ld	hl,0
	ld	(VAL),hl	; default index is 0
	xor	a
	ld	(EVMODE),a
	ld	c,2		; code 2 = (rp)
	ld	hl,(REGVAL)
	ld	a,l
	or	a
	ret	z		; return if (BC)
	cp	2
	ret	z		; return if (DE)
	ld	a,h
	or	a
	jr	nz,EVRP3	; skip next test if (IX) or (IY)
	call	GNC
	cp	')'
	pop	de
	ld	a,c
	ret	z		; return if no expression follows
	push	de
	call	BACKUP
	ld	a,(CPU)
	cp	2		; neither (SP) nor (HL) can have index
	ret	nz		;  in Z80/Z180 mode
EVRP3:	call	EVALNEW		; get index
	ld	c,4		; code 4 = (rp+d16)
	ld	de,(REGVAL)
	ld	a,e
	cp	6
	ret	z		; SP requires 16-bit index
	ld	a,d
	or	a
	jr	nz,EVRP4	; index is always returned for index register
	ld	a,(EVMODE)
	or	a
	ld	c,4		; code 4 = (rp+d16)
	ret	nz
	ld	a,h
	or	l
	ret	nz
	ld	c,2		; code 2 = (rp)
	ret			; return code 2 if HL displacement is zero
EVRP4:	ld	c,3		; code 3 = (rp+d8)
	ld	a,(EVMODE)
	ld	b,a
	call	CHK8U		; CHK8S? depends... I've seen code with
	ret	z		;          (IX+0FFh) in place of (IX-1)
	inc	c		; code 4 = (rp+d16)
	ld	a,(CPU)
	cp	2		; Z280 only
	ret	z
	dec	c		; back to code 3 = (rp+d8)
	ld	a,b
	or	a
	jp	nz,RELERR
	jp	VALERR		; no 16-bit IX/IY index in Z80/Z180 mode
;
;	Evaluate a register expression. If no valid register
;	is found, evaluate the expression as usual.
;
EVALREG:ld	de,REGS		; register names
EVAL0:	call	GNC		; skip blanks and get next char
	push	af
	call	BACKUP		; backup to start of word
	pop	af
	call	VALID1
	jp	c,EVALNEW
	xor	a
	ld	(PCFLAG),a	; reset PC relative flag
	ld	(EVFLGS),a	; reset flags from last search
	ld	(EVMODE),a
	ld	hl,0
	ld	(VAL),hl	; set val to 0
	ld	hl,(PTR1)
	push	hl
	call	ID		; get name
	call	IDTOUP		; ...to UPPERCASE
	ld	c,2
	call	SYMLUK		; lookup in table
	ld	a,(hl)		; fetch value
	inc	hl
	ld	h,(hl)
	ld	l,a
	ld	(INTBUF),hl
	ld	(VAL),hl
	pop	bc
	ld	a,(EVFLGS)
	ret	nc		; return if found with HL=(VAL) and A=(EVFLGS)
	ld	(PTR1),bc
	jp	EVALNEW
;
;	Like EVALREG, but also searches for special register names.
;
EVALSRG:ld	a,(CPU)
	ld	de,SREGS
	cp	2
	jr	z,EVAL0		; full table for Z280 special registers
	ld	de,SRGZ80
	jr	EVAL0		; reduced for Z80/Z180
;
SREGS:	defb	2+RSNAME
	defm	'PC'
	defb	3,0
	defb	4+RSNAME
	defm	'DEHL'
	defb	1,0
	defb	3+RSNAME
	defm	'USP'
	defb	2,0
SRGZ80:	defb	2+RSNAME
	defm	'AF'
	defb	6,0
REGS:	defb	1+RNAME
	defm	'A'
	defb	7,0
	defb	1+RNAME
	defm	'B'
	defb	0,0
	defb	1+RNAME
	defm	'C'
	defb	1,0
	defb	1+RNAME
	defm	'D'
	defb	2,0
	defb	1+RNAME
	defm	'E'
	defb	3,0
	defb	1+RNAME
	defm	'H'
	defb	4,0
	defb	1+RNAME
	defm	'L'
	defb	5,0
	defb	3+RNAME
	defm	'IXH'
	defb	4,0DDh
	defb	3+RNAME
	defm	'IXL'
	defb	5,0DDh
	defb	3+RNAME
	defm	'IYH'
	defb	4,0FDh
	defb	3+RNAME
	defm	'IYL'
	defb	5,0FDh
	defb	2+RPNAME
	defm	'HL'
	defb	4,0
	defb	2+RPNAME
	defm	'BC'
	defb	0,0
	defb	2+RPNAME
	defm	'DE'
	defb	2,0
	defb	2+RPNAME
	defm	'SP'
	defb	6,0
	defb	2+RPNAME
	defm	'IX'
	defb	4,0DDh
	defb	2+RPNAME
	defm	'IY'
	defb	4,0FDh
	defb	0
;
;	Evaluate a jump/call/return conditional expression.
;	If no conditional is found, evaluate a label expression.
;
EVALCND:ld	de,CONDS	; conditionals table
	jp	EVAL0
;
CONDS:	
	defb	1+CONDD
	defm	'Z'
	defb	1,0
	defb	2+CONDD
	defm	'AZ'
	defb	1,0
	defb	2+CONDD
	defm	'FZ'
	defb	1,0
	defb	2+CONDD
	defm	'LZ'
	defb	1,0
	defb	2+CONDD
	defm	'NZ'
	defb	0,0
	defb	3+CONDD
	defm	'ANZ'
	defb	0,0
	defb	3+CONDD
	defm	'FNZ'
	defb	0,0
	defb	3+CONDD
	defm	'LNZ'
	defb	0,0
	defb	1+CONDD
	defm	'C'
	defb	3,0
	defb	3+CONDD
	defm	'LLT'
	defb	3,0
	defb	2+CONDD
	defm	'NC'
	defb	2,0
	defb	3+CONDD
	defm	'LGE'
	defb	2,0
	defb	2+CONDD
	defm	'PO'
	defb	4,0
	defb	2+CONDD
	defm	'PE'
	defb	5,0
	defb	2+CONDD
	defm	'NV'
	defb	4,0
	defb	1+CONDD
	defm	'V'
	defb	5,0
	defb	1+CONDD
	defm	'P'
	defb	6,0
	defb	3+CONDD
	defm	'AGE'
	defb	6,0
	defb	3+CONDD
	defm	'FGE'
	defb	6,0
	defb	1+CONDD
	defm	'M'
	defb	7,0
	defb	3+CONDD
	defm	'ALT'
	defb	7,0
	defb	3+CONDD
	defm	'FLT'
	defb	7,0
	defb	2+CONDD
	defm	'NS'
	defb	6,0
	defb	1+CONDD
	defm	'S'
	defb	7,0
	defb	0
;
;	Evaluate an expression. If a symbol is found, search
;	symbol table only.
;
EVALNEW:ld	hl,(SYMTBL)	; start of symbol table
	ld	(SYMPT),hl
	; continue below
;
;	Evaluate an expression. If a symbol is found, search
;	using default symbol table pointer in (SYMPT).
;
;	Valid operators are: +, -, *, /, %, brackets and
;	extended ops (see table further below)
;
;	Valid elements are: ID's, numbers, string constants
;	and '$' for PC.
;
EVAL:	xor	a
	ld	(PCFLAG),a	; reset PC relative flag
	ld	(EVFLGS),a	; reset flags from last search
	ld	(EVMODE),a	; reset mode bits
	ld	(OPMODE),a
	ld	(VOID),a
	ld	(OPSTK),a	; initialize operator
	ld	(VALSTK),a	;  and operand stacks
	inc	a
	ld	(UNARY),a	; set unary flag
	ld	hl,0
	ld	(VAL),hl	; init val to 0
;	ld	(OPCOMN),hl
EVAL1:	call	GNC		; get next element or operator
	or	a
	jr	z,EVAL5		; end of line
	cp	','		;  or comma
	jr	z,EVAL4		;   terminates expression
	ld	(VOID),a
	cp	'0'
	jr	c,EVAL3
	cp	'9'+1
	jp	c,NUMBER	; branch if number
EVAL3:	call	VALID1
	jp	nc,EVAL13	; branch if ID or PC ref
	cp	'('
	jp	z,LPAR
	cp	')'
	jp	z,RPAR
	cp	'+'
	jp	z,PLUS
	cp	'-'
	jp	z,MINUS
	cp	'*'
	jp	z,MULTPLY
	cp	'/'
	jp	z,DIVIDE
	cp	'%'		; modulo
	jp	z,MODULO
	cp	'&'		;& == AND
	jr	nz,EVAL3A
SPEXOPS:			;"special" operator
	ld	(IDBUF),a	;store-it as ID
	ld	a,1
	ld	(IDLEN),a
	jp	EXOPS		
EVAL3A:	cp	'<'		;< == LT
	jr	z,SPEXOPS
	cp	'>'		;> == GT
	jr	z,SPEXOPS
	cp	'='		;= == EQU
	jr	z,SPEXOPS
	cp	'^'		;^ == OR
	jr	z,SPEXOPS
	cp	'.'
	jr	z,DOTSPEC
	cp	27H		;'
	jp	z,QCONST	; process quoted char constant
	cp	'"'
	jp	z,QCONST	; process quoted char constant
EVAL4:	call	BACKUP
EVAL5:	ld	a,(VOID)
	or	a
	jp	z,OERROR
EVAL6:	ld	hl,OPSTK
	call	STKPOP
	jr	c,EVAL7		; exit loop if operator stack empty
	ld	a,e
	cp	OPLPAR
	jp	z,EXPERR	; error if operator is left bracket
	ld	b,c
	ld	c,e
	call	APPLY		; apply operator
	jr	EVAL6		; loop to process next
EVAL7:	ld	hl,VALSTK
	call	STKPOP		; pop result from value stack
	jr	nc,EVAL7A
	ld	de,0		; if stack was empty, result is zero
	ld	c,0
EVAL7A:	ld	a,(VALSTK)
	or	a		; stack must be empty
	jp	nz,EXPERR	; else we have an unbalanced expression
;	ld	hl,(OPCOMN)
;	ld	(CMNPTR),hl
	ex	de,hl
	ld	a,c
	ld	(EVMODE),a
	ld	a,(EVFLGS)
	ld	(VAL),hl
	ret			; get out of here, we're done (note CY clear)
;
DOTSPEC:			;store chars into IDBUF until next '.'
	push	af
	xor	a
	ld	(IDLEN),a
	ld	hl,IDBUF
	pop	af
STRC:	ld	(hl),a
	inc	hl
	ld	a,(IDLEN)
	inc	a
	ld	(IDLEN),a
	call	GNC
	or	a
	jp	z,OERROR
	cp	'.'
	jr	nz,STRC
	ld	(hl),a		;store final '.'
	inc	hl
	ld	a,(IDLEN)
	inc	a
	ld	(IDLEN),a	
SRCH:				;then search possible special operators
	ld	de,DOTOPS	; dot ops table
	ld	c,0
	call	SYMLUK		; search in table
	jp	c,OERROR
	jp	EXOPS
;
DOTOPS:
	defb	5
	defm	'.and.'
	defb	4
	defm	'.eq.'
	defb	4
	defm	'.gt.'
	defb	6
	defm	'.high.'
	defb	5
	defm	'.low.'
	defb	4
	defm	'.lt.'
	defb	5
	defm	'.mod.'
	defb	5
	defm	'.not.'
	defb	4
	defm	'.or.'
	defb	5
	defm	'.shl.'
	defb	5
	defm	'.shr.'
	defb	5
	defm	'.ult.'
	defb	5
	defm	'.ugt.'
	defb	5
	defm	'.xor.'
	defb	0
;
;	Process number
;
NUMBER:	call	NUMfb		;check if it is an ID type 'dddddf' or 'dddddb'
	jr	nc,ISID		;if yes, process the ID
	call	BACKUP
	call	INT		; convert to binary using the current base
ASTORE:	xor	a
	ld	(OPMODE),a	; mode = Absolute
STORE:	ld	hl,VALSTK
	ld	de,(INTBUF)
	ld	a,(OPMODE)
	ld	c,a
	call	STKPUSH		; push value and mode
	jp	c,EXPERR	; test for stack overflow
	xor	a
	ld	(UNARY),a	; clear unary flag
	jp	EVAL1
;
;	Process '$' ref
;
PCREF:	ld	a,(DSFLAG)
	or	a
	jr	z,nodefs
	xor	a		; if DEFS ... $ ... is used
	ld	(JFLAG),a	; disable jump optimization
nodefs:
	call	GETPPC		; get effective PC value
	ld	a,(DBWFLG)	; are we processing DB/DW statement?
	or	a
	jr	z,NODB		; branch if not
	ld	a,(LEN)		; get length so far
	ld	e,a
	ld	d,0
	add	hl,de		; adjust PC value accordingly
NODB:	ld	(INTBUF),hl
	ld	a,(CURSEG)
	ld	(OPMODE),a
;	cp	0C0h
;	ld	de,(CURCMN)
;	call	z,COMNM
	ld	a,(PCFLAG)	; fetch PC relative value flag
	inc	a
	ld	(PCFLAG),a	; set flag
	jr	STORE
;
;	Process ID
;
EVAL13:	call	BACKUP
	call	ID		; get identifier name
ISID:
	ld	a,(IDLEN)	; check length
	dec	a		; single char?
	jr	nz,EVAL14	; branch if not
	ld	a,(IDBUF)
	cp	'$'
	jr	z,PCREF		; branch if PC ref
EVAL14:	call	CHKEXT		; check for external##
	jr	z,EVAL15
	ld	de,(SYMPT)
	ld	c,3		;5
	call	SYMLUK		; lookup identifier
	jr	c,EXOPS		; try extended ops if undefined
	push	hl
	inc	hl		;skip val
	inc	hl
	ld	a,(hl)		;mode
	and	EXTSYM		;external?
	jr	z,notext
	ld	(LASTEXTSYM),de	;yes, save external symbol address
notext:	pop	hl
EVAL15:	ld	a,(PCFLAG)	; fetch PC relative flag
	inc	a
	ld	(PCFLAG),a	; set flag to mark as PC-relative
	ld	c,l
	ld	b,h
	ld	e,(hl)		; get value
	inc	hl
	ld	d,(hl)
	inc	hl
	ld	a,(hl)		; and mode
	ld	(OPMODE),a
EVAL16:	ld	(INTBUF),de
	ld	a,(OPMODE)
	and	UNDEF		; undefined bit set?
	jp	z,STORE		; store value if not
EUNDEF:	ld	hl,0		; else the respression result is undefined
	ld	(INTBUF),hl	; set value to 0
	ld	a,UNDEF
	ld	(OPMODE),a
	ld	a,(UFLAG)
	or	1
	ld	(UFLAG),a	; set undefined flag
	jp	STORE
;
;	Check for External declaration of type 'label##'
;
CHKEXT:	ld	hl,(PTR1)
	ld	a,(hl)
	cp	'#'
	ret	nz
	inc	hl
	ld	a,(hl)
	cp	'#'
	ret	nz
	inc	hl
	ld	(PTR1),hl
	call	ADDEXT		; add new external reference
	ld	hl,(SYMADR)	; TODO: handle error? ADDEXT may fail!
	xor	a
	ret			; return with Z flag set if found
;
;	Decode extended operators
;
EXOPS:	ld	de,EXTOPS	; extended operands
	ld	c,3
	call	SYMLUK		; search in table
	jr	c,EXOPER	; branch if not found
	ld	e,(hl)		; get op number
	inc	hl
	ld	d,(hl)		; get precedence
	inc	hl
	ld	b,(hl)		; get unary flag
	ld	a,e
	cp	21		; NUL is special
	jr	z,BNUL
	ex	de,hl
	ld	a,b
	or	a
	jp	z,EVAL27	; jump if not unary (binary)
	ld	a,(UNARY)
	or	a		; if unary, see if allowed by syntax rules
	jp	nz,EVL27B	; go process it if yes
	jp	EXPERR		; else error
;
BNUL:	ld	hl,-1		; preset true
BN1:	call	GNC
	or	a
	jr	z,BN2
	ld	hl,0		; false
	jr	BN1		; NUL consumes everything up to end of line
BN2:	ld	(INTBUF),hl
	jp	ASTORE
;
EXOPER:	ld	a,(UMODE)	; treat undefined labels as External?
	or	a
	jr	z,EUNDEF	; branch if not, leave undefined
	ld	hl,0
	ld	(VAL),hl
	ld	a,UNDEF		; else enter symbol as undefined label
	ld	(SYMMOD),a	;  (will be converted to External later)
	call	ADDSYM		; TODO: handle error?
	ld	hl,(SYMADR)
	jp	EVAL15
;
;	Extended operators
;
;	defb	length,name,value,precedence,unary_flag
;
EXTOPS:	defb	3		;#1
	defm	'NOT'
	defb	6,3,1
	defb	5		;#2
	defm	'.not.'		;.not. == NOT
	defb	6,3,1
	defb	3		;#3
	defm	'MOD'
	defb	5,7,0
	defb	5		;#4
	defm	'.mod.'		;.mod. == MOD
	defb	5,7,0
	defb	3		;#5
	defm	'SHR'
	defb	7,7,0
	defb	5		;#6
	defm	'.shr.'		;.shr. == SHR
	defb	7,7,0
	defb	3		;#7
	defm	'SHL'
	defb	8,7,0
	defb	5		;#8
	defm	'.shl.'		;.shl. == SHL
	defb	8,7,0
	defb	3		;#9
	defm	'AND'
	defb	9,2,0
	defb	5		;#10
	defm	'.and.'		;.and. == AND
	defb	9,2,0
	defb	1		;#11
	defm	'&'		;& == AND
	defb	9,2,0
	defb	2		;#12
	defm	'OR'
	defb	10,1,0
	defb	4		;#13
	defm	'.or.'		;.or. == OR
	defb	10,1,0
	defb	1		;#14
	defm	'^'		;^ == OR
	defb	10,1,0
	defb	3		;#15
	defm	'XOR'
	defb	11,1,0
	defb	5		;#16
	defm	'.xor.'		;.xor. == XOR
	defb	11,1,0
	defb	2		;#17
	defm	'EQ'
	defb	12,4,0
	defb	4		;#18
	defm	'.eq.'		;.eq. == EQ
	defb	12,4,0
	defb	1		;#19
	defm	'='		;= == EQ
	defb	12,4,0
	defb	2		;#20
	defm	'NE'
	defb	13,4,0
	defb	2		;#21
	defm	'LT'
	defb	14,4,0
	defb	5		;#22
	defm	'.ult.'		;.ult. == LT
	defb	14,4,0
	defb	2		;#23
	defm	'LE'
	defb	15,4,0
	defb	2		;#24
	defm	'GT'
	defb	16,4,0
	defb	5		;#25
	defm	'.ugt.'		;.ugt. == GT
	defb	16,4,0
	defb	4		;#26
	defm	'.gt.'		;.gt. == GT
	defb	16,4,0
	defb	1		;#27
	defm	'>'		;> == GT
	defb	16,4,0
	defb	2		;#28
	defm	'GE'
	defb	17,4,0
	defb	3		;#29
	defm	'LOW'
	defb	18,8,1
	defb	5		;#30
	defm	'.low.'		;.low. == LOW
	defb	18,8,1
	defb	4		;#31
	defm	'HIGH'
	defb	19,8,1
	defb	6		;#32
	defm	'.high.'	;.high. == HIGH
	defb	19,8,1
	defb	4		;#33
	defm	'LESS'
	defb	20,4,0
	defb	1		;#34
	defm	'<'		;< == LESS
	defb	20,4,0
	defb	4		;#35
	defm	'.lt.'		;.lt. == LESS
	defb	20,4,0
	defb	3		;#36
	defm	'NUL'
	defb	21,9,0
	defb	0		
OPLPAR	equ	36		;22
;
;	Process operators
;
PLUS:	ld	a,(UNARY)
	or	a
	jp	nz,EVAL1	; unary +, ignore
	ld	hl,5*256+1	; +
	ld	b,0
	jr	EVAL27
;
MINUS:	ld	a,(UNARY)
	or	a
	ld	hl,6*256+0	; unary -
	ld	b,1
	jr	nz,EVL27B
	ld	hl,5*256+2	; -
	ld	b,0
	jr	EVAL27
;
MULTPLY:ld	hl,7*256+3	; *
	ld	b,0
	jr	EVAL27
;
DIVIDE:	ld	hl,7*256+4	; /
	ld	b,0
	jr	EVAL27
;
MODULO:	ld	hl,7*256+5	; % or MOD
	ld	b,0
	jr	EVAL27
;
;	Here with:
;	H = operator precedence
;	L = operator code
;	B = unary flag
;
EVAL27:	push	hl
	ld	hl,OPSTK
	call	STKPOP		; pop operator
	pop	hl
	jr	c,EVL27B	; exit loop if stack empty
	ld	a,e
	cp	OPLPAR		; left bracket?
	jr	z,EVL27A	; exit loop if yes
	ld	a,d		; get precedence
	cp	h		; compare with current operator
	jr	c,EVL27A	; exit loop if lower
	push	hl
	push	bc
	ld	b,c
	ld	c,e
	call	APPLY		; else apply current operator
	pop	bc
	pop	hl
	jr	EVAL27		; and loop to test another one
EVL27A:	push	hl
	ld	hl,OPSTK
	call	STKPUSH		; push operator back
	pop	hl
EVL27B:	ex	de,hl
	ld	c,b
	ld	hl,OPSTK
	call	STKPUSH		; push new operator
	ld	a,1
	ld	(UNARY),a	; next +/- is unary
	jp	EVAL1
;
;	Process left bracket
;
LPAR:	ld	de,OPLPAR	; (
	ld	c,1
	ld	hl,OPSTK
	call	STKPUSH
	ld	a,1
	ld	(UNARY),a	; next +/- is unary
	jp	EVAL1
;
;	Process right bracket
;
RPAR:	ld	hl,OPSTK
	call	STKPOP		; pop operator
	jp	c,EVAL4		; exit EVAL if one ) too many (probably OK!)
	ld	a,e
	cp	OPLPAR		; left bracket?
	jr	z,RP1		; exit loop if yes
	ld	b,c
	ld	c,e
	call	APPLY		; apply operator
	jr	RPAR		; loop until left bracket found
RP1:	xor	a
	ld	(UNARY),a	; next +/- is binary
	jp	EVAL1
;
;	INT - Convert characters to binary
;
;	Allow trailing 'H' for hex, 'O' or 'Q' for octal, 'D' for decimal
;	and 'B' for binary. Default base is the current .RADIX setting.
;
INT:	ld	hl,(RADIX)
	ld	(BASE),hl	; set up default base
	ld	b,0		; set up length counter
	call	GNC		; skip blanks
	ld	hl,(PTR1)
	dec	hl		; HL = begin of string
	push	hl		; save pointer
INT1:	cp	'0'
	jr	c,INT3		; exit loop if terminator
	cp	'9'+1
	jr	c,INT2
	cp	'A'
	jr	c,INT3
	cp	'Z'+1
	jr	nc,INT3
INT2:	inc	hl
	ld	a,(hl)		; get next character
	call	UCASE
	inc	b
	jr	INT1		; increment counter and continue loop
;
INT3:	ld	(PTR1),hl
	dec	hl
	ld	a,(hl)
	call	UCASE
	cp	'H'
	ld	hl,16		; base 16
	jr	z,INT4		; branch if hex
	cp	'D'
	ld	hl,10		; base 10
	jr	z,INT4		; branch if decimal
	cp	'Q'
	ld	hl,8		; base 8
	jr	z,INT4		; branch if octal
	cp	'O'
	jr	z,INT4		; branch if octal
	cp	'B'
	ld	hl,2		; base 2
	jr	nz,INT5		; branch if not binary
INT4:	dec	b		; decrement counter
	ld	(BASE),hl	; set up base
INT5:	ld	de,0		; set up accumulator
INT6:	pop	hl		; get saved pointer to string
	ld	a,(hl)
	inc	hl
	push	hl
	call	UCASE
	cp	'A'
	jr	c,INT7
	sub	7		; for A-F
INT7:	sub	'0'
	push	af		; get binary value of this digit
	ld	hl,(BASE)
	cp 	l		; ensure that digit value is < BASE
	call	nc,VALERR	; else set error flag, but continue anyway
	call	MULT
	pop	af
	ld	e,a
	ld	d,0
	add	hl,de		; add in new digit
	ex	de,hl
	djnz	INT6		; go back if more to do
	pop	hl		; drop pointer to string
	ex	de,hl
	ld	(INTBUF),hl	; save value
	ret
;
;	Apply operator to value(s) from stack
;	On entry, C contains operator code and B the unary flag
;
APPLY:	xor	a
	ld	(EVMODE),a
	ld	hl,VALSTK
	push	hl
	push	bc
	call	STKPOP
	ld	a,c
	pop	bc
	pop	hl
	jp	c,EXPERR
	ld	(INTBUF),de	; INTBUF = second operand
	ld	(OPMODE),a
	ld	a,b
	or	a
	jr	nz,BSKIP	; jump if unary (only one operand)
	push	bc
	call	STKPOP
	ld	a,c
	pop	bc
	jp	c,EXPERR
	ld	(VAL),de	; VAL = first operand
	ld	(EVMODE),a
BSKIP:	ld	hl,OPRET	; where to return to after function
	push	hl		; put return address on stack
	ld	a,c		; get operator number
	add	a,a		; double for word index
	ld	e,a		; index into table with it
	ld	d,0
	ld	hl,OPTAB	; table of operation addreses
	add	hl,de
	ld	e,(hl)
	inc	hl
	ld	d,(hl)
	push	de		; we are going there via a return
				;  so push address on stack
	ld	de,(VAL)	; now get the operands
	ld	hl,(INTBUF)
	ret			; and go to it!
;
OPRET:	ex	de,hl
	ld	hl,VALSTK
	ld	a,(EVMODE)
	ld	c,a
	; fall thru - push result value and mode
;
;	Push word value onto stack
;	On entry, HL points to stack structure (OPSTK or VALSTK),
;	DE contains value to push, C contains mode
;	Preserves BC
;
STKPUSH:ld	a,(hl)		; get index
	cp	OSTKSZ		; test for stack overflow
	scf
	ret	z		; on error, return with CY set
	push	bc
	ld	c,a
	ld	b,0
	inc	a
	ld	(hl),a
	inc	hl		; point to stack area
	add	hl,bc		; index into stack
	add	hl,bc
	add	hl,bc
	ld	(hl),e		; push value
	inc	hl
	ld	(hl),d
	pop	bc
	inc	hl
	ld	(hl),c		; push mode
	xor	a
	ret
;
;	Pop word value from stack
;	On entry, HL points to stack structure (OPSTK or VALSTK)
;	Upon return, DE and C contains value and mode respectively
;	Preserves B
;
STKPOP:	ld	a,(hl)		; get index
	or	a		; test for stack underflow 
	scf
	ret	z		; on error, return with CY set
	push	bc
	dec	a
	ld	(hl),a
	inc	hl		; point to stack area
	ld	c,a
	ld	b,0
	add	hl,bc		; index into stack
	add	hl,bc
	add	hl,bc
	ld	e,(hl)		; pop value
	inc	hl
	ld	d,(hl)
	inc	hl
	pop	bc
	ld	c,(hl)
	xor	a
	ret
;
;	Operator dispatch table
;
OPTAB:	defw	UMIN		;  0: unary minus
	defw	BADD		;  1: add
	defw	BSUB		;  2: subtract
	defw	BMULT		;  3: multiply
	defw	BDIV		;  4: divide
	defw	BMOD		;  5: modulo
	defw	UNOT		;  6: unary NOT
	defw	BSHR		;  7: shift right
	defw	BSHL		;  8: shift left
	defw	BAND		;  9: logical AND
	defw	BOR		; 10: logical OR
	defw	BXOR		; 11: logical exclusive OR
	defw	BEQ		; 12: equal
	defw	BNE		; 13: not equal
	defw	BLT		; 14: less than
	defw	BLE		; 15: less than or equal
	defw	BGT		; 16: greater than
	defw	BGE		; 17: greater than or equal to
	defw	ULOW		; 18: unary low byte (MOD 256)
	defw	UHIGH		; 19: unary high byte (SHR 8)
	defw	BLESS		; 20: signed less than
;
;	Unary minus
;
UMIN:	call	UNOT
	inc	hl
	ret
;
;	Add
;
BADD:	call	ADDM
	add	hl,de
	ret
;
;	Subtract
;
BSUB:	call	SUBM
	ex	de,hl
	or	a
	sbc	hl,de		; HL = DE-HL
	ret
;
;	Multiply:  HL <= HL * DE (16 bits only)
;
BMULT:	call	MIXM
MULT:	push	bc
	ld	c,l
	ld	b,h
	ld	hl,0
	ld	a,16
MU1:	add	hl,hl
	rl	e
	rl	d
	jr	nc,MU2
	add	hl,bc
	jr	nc,MU2
	inc	de
MU2:	dec	a
	jr	nz,MU1
	pop	bc
	ret
;
;	Divide
;
;	Divides 16-bit dividend by 16-bit divisor
;	On entry:  DE = dividend, HL = divisor
;	On return: HL = quotient, DE = remainder
;
BDIV:	call	MIXM
	ld	a,h		; test for 0 divisor
	or	l
	jr	nz,BDIV0	; jp if not /0
	ld	a,'Z'
	ld	(ERRFLG),a	; flag divide by 0 error
	scf			; mark 0 divisor as error
	ret			; ...and exit
;
BDIV0:	push	bc
	ld	c,l
	ld	b,h
	ld	hl,0
	ld	a,16
DIV1:	rl	e
	rl	d
	adc	hl,hl
	sbc	hl,bc
	jr	nc,DIV2
	add	hl,bc
DIV2:	ccf
	dec	a
	jr	nz,DIV1
	rl	e
	rl	d
	ex	de,hl
	pop	bc
	ret
;
EXPERR:	ld	a,'E'
	ld	(ERRFLG),a	; flag expression error
	scf
	ret			; exit this line
;
;	Modulo
;
BMOD:	call	BDIV
	ex	de,hl		; DE has modulo
	ret
;
;	Process single/double byte in quotes
;
QCONST:	ld	c,a		; save quote char in C
	ld	hl,(PTR1)
	call	CCONST		; get character constant
	jr	c,EXPERR
	ld	(PTR1),hl
	ld	(INTBUF),de	; store result
	jp	STORE		; and go process value normally
;
;	Get single/double byte character constant
;
CCONST:	ld	de,0		; init result
	ld	a,(hl)		; get character
	or	a
	scf
	ret	z
	inc	hl
	cp	c
	jr	nz,CC1		; not double quotes
	ld	a,(hl)		; fetch next char
	cp	c
	jr	nz,CC3		; not double quotes
	inc	hl
CC1:	ld	e,a
	ld	a,(hl)		; fetch second byte
	or	a
	scf
	ret	z
	inc	hl
	cp	c
	jr	nz,CC2
	ld	a,(hl)		; fetch next char
	cp	c
	jr	nz,CC3		; not double quotes -> 1 byte value
	inc	hl
CC2:	ld	d,a		; make 2 byte value
	ld	a,(hl)
	or	a
	scf
	ret	z
	inc	hl
	cp	c		; check for closing quote
	scf
	ret	nz
CC3:	xor	a		; return success
	ret
;
;	UNOT - Logical complement
;
UNOT:	call	UNARYM
	ld	a,h		; complement result
	cpl
	ld	h,a
	ld	a,l
	cpl
	ld	l,a
	ret
;
;	BLESS - Signed less than
;
BLESS:	call	BSUB
	ld	hl,-1
	jp	pe,B1		; branch on overflow
	ret	m		; if left LESS right
	inc	hl
	ret
B1:	ret	p
	inc	hl
	ret
;
;	BLE - Less than or equal
;
BLE:	call	BSUB
	jr	c,BLE1		; if left LT right
	ld	a,h
	or	l
	ld	hl,-1
	ret	z		; if left EQ right
	inc	hl
	ret
BLE1:	ld	hl,-1
	ret
;
;	BLT - Less than
;
BLT:	call	BSUB
	ld	hl,-1		; preset true
	ret	c		; if left LT right
	inc	hl
	ret
;
;	BEQ - Equal
;
BEQ:	call	BSUB
	ld	a,h
	or	l
	ld	hl,-1
	ret	z		; if left EQ right
	inc	hl
	ret
;
;	BGT - Greater than
;
BGT:	call	BSUB
	jr	c,BGT1		; if left LT right
	ld	a,h
	or	l
	ret	z		; if left EQ right
	ld	hl,-1
	ret
BGT1:	ld	hl,0
	ret
;
;	BGE - Greater than or equal
;
BGE:	call	BSUB
	ld	hl,0		; preset false
	ret	c		; if left LT right
	dec	hl		; true
	ret
;
;	BNE - Not equal
;
BNE:	call	BEQ		; compare args
	ld	a,l		; complement result
	cpl
	ld	l,a
	ld	a,h		; complement result
	cpl
	ld	h,a
	ret
;
;	BAND - Logical product
;
BAND:	call	MIXM
	ld	a,e
	and	l
	ld	l,a
	ld	a,d
	and	h
	ld	h,a
	ret
;
;	BOR - Logical sum
;
BOR:	call	MIXM
	ld	a,e
	or	l
	ld	l,a
	ld	a,d
	or	h
	ld	h,a
	ret
;
;	BXOR - Logical difference
;
BXOR:	call	MIXM
	ld	a,e
	xor	l
	ld	l,a
	ld	a,d
	xor	h
	ld	h,a
	ret
;
;	BSHR - Shift right
;
BSHR:	call	UNARYM
	ex	de,hl
BSHR1:	ld	a,d
	or	e
	ret	z
	srl	h
	rr	l
	dec	de
	jr	BSHR1
;
;	BSHL - Shift left
;
BSHL:	call	UNARYM
	ex	de,hl
BSHL1:	ld	a,d
	or	e
	ret	z
	add	hl,hl
	dec	de
	jr	BSHL1
;
;	ULOW - Low byte
;
ULOW:	call	UNARYM
	ld	h,0
	ret
;
;	UHIGH - High byte
;
UHIGH:	call	UNARYM
	ld	l,h
	ld	h,0
	ret

	SUBTTL	'Relocation rules'
;
; All symbols are in one of the following modes, depending on the
; segment under which they are defined:
;   * Absolute
;   * Code relative
;   * Data relative
;   * COMMON
;   * External (segment unknown, resolved only at link phase)
;
; In addition, two COMMON symbols are not in the same mode unless they
; are in the same COMMON block.
;
; Mode rules:
;
; 1. If the operation is addition:
;    a) at least one of the operands must be Absolute
;    b) Absolute + <mode> = <mode>
;
; 2. If the operation is subtraction:
;    a) <mode> - Absolute = <mode>
;    b) <mode> - <mode> = Absolute, where the two <mode>s are the same
;
; 3. In any operation other than addition or subtraction, the mode of
;    both operands must be Absolute.
;
; External rules:
;
; 1. External values are assembled always to a word field.
;
; 2. Externals are legal only in addition and subtraction.
;
; 3. If an External symbol is used in an expression, the result is
;    always External.
;
; 4. If the operation is addition:
;    a) either operand (but not both) may be External.
;
; 5. If the operation is subtraction:
;    a) only the first operand may be External.

;
;	Check addition relocation mode
;
ADDM:	call	ADDEX		; check External bits
	ld	a,(OPMODE)
	and	SEGMASK		; check operator mode
	ret	z		; return if Absolute
	ld	c,a
	ld	a,(EVMODE)
	ld	b,a
	and	SEGMASK		; else check current evaluation mode
	jr	nz,RELERR	; error if not Absolute (rel+rel not allowed)
	ld	a,b
	or	c
	ld	(EVMODE),a	; set result mode
	ret
;
ADDEX:	ld	a,(OPMODE)
	and	EXTSYM		; check if operator is External
	ret	z		; return if not
	ld	c,a
	ld	a,(EVMODE)
	ld	b,a
	and	EXTSYM		; else check current evaluation
	jr	nz,RELERR	; error if External (ext+ext not allowed)
	ld	a,b
	or	c
	ld	(EVMODE),a	; result mode is External
	ret
;
;	Check subtraction relocation mode
;
SUBM:	call	SUBEX		; check External bits
	ld	a,(OPMODE)
	and	SEGMASK		; check operator mode
	ret	z		; return if Absolute
	ld	c,a
	ld	a,(EVMODE)
	and	SEGMASK		; else check current evaluation mode
	cp	c
	jr	nz,RELERR	; error if not same mode
	xor	a
	ld	(EVMODE),a	; else result just became Absolute
	ret
;
SUBEX:	ld	a,(OPMODE)
	and	EXTSYM		; check if operator is External
	ret	z		; return if not
	jr	RELERR		; else error (only 1st operand can be External)
;
;	Check unary relocation mode
;
UNARYM:	ld	a,(OPMODE)
	and	SEGMASK .or. EXTSYM	; 11x1xxxx - seg type and External bit
	ret	z
	jr	RELERR		; operand must be Absolute
;
;	Check relocation mode for other operations
;
MIXM:	ld	a,(OPMODE)
	and	SEGMASK .or. EXTSYM	; 11x1xxxx - seg type and External bit
	ld	c,a
	ld	a,(EVMODE)
	and	SEGMASK .or. EXTSYM
	or	c
	ret	z		; both operands must be Absolute
RELERR:	ld	a,'R'
	ld	(ERRFLG),a
	ret
;
;	Strictly speaking, we need to push the COMMON block address
;	together with the symbol value and segment type onto the value
;	stack, as it is part of the expression mode.
;	However, since inter-segment operations are not allowed anyway,
;	we can get away with that by simply keeping a single pointer.
;	Every time we find a reference to a COMMON block we compare the
;	corresponding COMMON block address to the one saved earlier,
;	and if they differ we set an error flag.
;
;	There are a few cases when this will result in a false error
;	condition, but there are ways to get around the problem, e.g.:
;
;		common	/c1/
;	aa:	defb	1,2,3
;	bb	equ	$
;		common	/c2/
;	cc	equ	$+(bb-aa)	; an error will be generated
;					;  even when the expr is legal
;	Workaround:
;
;		common	/c1/
;	aa:	defb	1,2,3
;	bb	equ	$
;	sz	equ	bb-aa		; expression computed before
;					;  switching to another COMMON
;		common	/c2/
;	cc	equ	$+sz		; OK.
;
;	Since COMMONs are not that common anyway, they are probably
;	not worth the effort of complicating (and making slower) the
;	code any further.
;
;COMNM:	ld	a,(OPMODE)
;	and	11000000B
;	cp	CMNSYM		; COMMON reference?
;	ret	nz		; return if not
;	ld	a,d
;	or	e		; valid COMMON block pointer?
;	ret	z		; return if not
;	ld	hl,(OPCOMN)
;	ld	a,h		; test saved pointer
;	or	l		; valid?
;	jr	z,CM1		; branch if not
;	call	CMPHD		; same?
;	ret	z		; return success
;	jr	RELERR		; else set error flag
;CM1:	ld	(OPCOMN),de	; set new pointer
;	ret

	psect	data

INTBUF:	defw	0		; returned value from INT routine
REGVAL:	defw	0		; register value from (rp+dd) expression
BASE:	defw	0		; base of number in INT convert loop
VOID:	defb	0
SYMPT:	defw	0
OPMODE:	defb	0		; mode of last operand
;OPCOMN:	defw	0		; address of COMMON block of last operand
UNARY:	defb	0		; unary +/- flag

OPSTK:	defb	0		; operator stack index
	defs	OSTKSZ*3	; stack space

VALSTK:	defb	0		; operand stack index
	defs	OSTKSZ*3	; stack space

;	END
