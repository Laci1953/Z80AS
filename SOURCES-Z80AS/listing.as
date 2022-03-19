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
	SUBTTL	Listing generation routines

*include	ZSM.INC

;public
	global	LSTINI,LSTOUT,LSTB,LSTW,CNV2HX,PLINE,PMSG
	global	SYMBOL,MACROS,MAXLNE
;extern
	global	GETPPC,MACCHK,FNDREC,FNDNXT,WERROR,WLINE,WNB

	global	HDRBUF,HOFEND,HOFPG,HOFMSG,ERRFLG,ERRCNT,MACFLG
	global	INCLVL,LEN,LENDS,LSTOPT,MACPTR,CONDSP,VAL,EQUFLG
	global	LBLFLG,DSFLAG,DBWFLG,CLINE,PC,EVMODE,CURSEG,CURLNE
	global	LFLAG,UFLAG,SYMPTR,SYMTBL,TITLEB,SBTTLB,IFLIST
	global	NOLIST

	psect	text
;
;	Clear line header (error, address and object code fields)
;
LSTINI:	ld	hl,HDRBUF
	ld	e,l
	ld	d,h
	inc	de
	ld	bc,HDRSZ-1
	ld	(hl),' '
	ldir
	xor	a
	ld	(de),a
	ld	(LSTCNT),a	; clear character count
	call	GETPPC		; get effective PC
	ld	a,(LEN)
	ld	e,a
	ld	d,0
	add	hl,de
	ld	(PCLINE),hl
	ret
;
;	Prepare line for listing
;
LSTPRE:	ld	a,(ERRFLG)
	cp	' '		; test for error
	jr	z,S403
	ld	hl,(ERRCNT)
	inc	hl		; incr count
	ld	(ERRCNT),hl
S403:	ld	hl,HDRBUF+HDRSZ-3
	ld	a,(INCLVL)	; test for INCLUDE file
	or	a
	jr	z,S406
	ld	(hl),'C'	; 'C' indicates this line is from an INCLUDE file
S406:	ld	a,(MACFLG)
	inc	hl
	ld	(hl),a		; '+' indicates this line is part of a MACRO expansion

	ld	de,HDRBUF
	ld	a,(ERRFLG)
	or	a
	res	7,a
	ld	(de),a		; store error code
	jp	m,S407		; skip conditional test if forced error output
	
	ld	hl,(CONDSP)
	ld	a,(hl)		; else get conditional state
	rra			;  into carry, set = true
	ret	nc		; return if not true, no code generated

S407:	ex	de,hl		; pointer to HDRBUF now in HL
	inc	hl
	inc	hl
	ld	a,(EQUFLG)
	or	a
	ld	de,(VAL)
	jr	nz,S405		; branch if to use VAL (EQU, ORG, END)

	ld	a,(LEN)
	ld	c,a
	ld	a,(DSFLAG)
	or	c
	ld	c,a
	ld	a,(LBLFLG)
	or	c
	ret	z		; do not output address if no code generated
	xor	a
	ld	de,(PCLINE)	; get effective PC value for this line

S405:	push	af		; remember EQUFLG value
	ld	a,d
	call	CNV2HX		; store address
	ld	a,e
	call	CNV2HX
	call	SEGCHR
	ld	(hl),c		; add segment type char
	inc	hl
	inc	hl
	pop	af		; pop EQUFLG value
	jr	nz,S410		; jump if EQU
	ld	a,(DSFLAG)
	or	a
	jr	nz,S411		; branch if to use LENDS

	ret

S410:	ld	(hl),'='
	ret

S411:	ld	(hl),'('
	inc	hl
	inc	hl
	ld	de,(LENDS)
	ld	a,d
	call	CNV2HX		; output DEFS/DS size
	ld	a,e
	call	CNV2HX
	inc	hl
	ld	(hl),')'
	ret
;
;	Process the listing output if required
;
LSTOUT:	ld	a,(UFLAG)	; undefined symbol
	or	a
	jr	z,S401B		; skip if not
	ld	a,'U'		; set up undefined error
	ld	(ERRFLG),a
	xor	a
	ld	(UFLAG),a
S401B:	ld	a,(ERRFLG)	; have we an error on this line?
	cp	' '
	jr	nz,S402		; always print errors
;
;	LFLAG:	Copy of list option from command line
;
	ld	a,(LFLAG)	; do we need to print this line?
	cp	'Z'
	ret	z		; don't print if user asked us not to
;
;	LISTFL: True if we want a listing
;
	ld	a,(LSTOPT)	; has the listing been turned off?
	bit	LISTFL,a	; Z = yes
	ret	z		; skip if listing turned off
;
;	NOLIST: True to avoid listing TITLE, FORM, PAGE, EJECT
;
	ld	a,(NOLIST)	; is this a TITLE, FORM,
	or	a		;  PAGE or EJECT pseudo-op?
	ret	nz		; we don't print those
;
	ld	a,(MACFLG)	; get MACRO state
	cp	' '		; ' ' = false
	jr	z,S401D		; skip if false
;
;	MACRFL: False to suppress listing of MACRO expansions
;
	ld	a,(LSTOPT)
	bit	MACRFL,a	; bit set = print MACRO expansions
	ret	z		; bit clear = don't
;
;	XMACFL: True to list only MACRO lines that generate object code
;
	bit	XMACFL,a	; bit set = print only lines that generate code
	jr	z,S401D		; bit clear = print all lines
;
	ld	a,(LEN)		; list line if object code is generated
	ld	c,a
	ld	a,(DSFLAG)
	or	c		; also if it contains DEFS statements
	ld	c,a
	ld	a,(LBLFLG)	; or if a label is defined on that line
	or	c
	ret	z
;
S401D:	ld	hl,(CONDSP)	; get conditional state
	ld	a,(hl)		; get state
	rra			;  into carry, set = true
	jr	nc,S401C	; skip if not true
;
;	IFLIST:	True to suppress listing on IF, ELSE, ENDIF
;		when "LIST NOCOND" is current.
;
	ld	a,(IFLIST)	; IF, ELSE, ENDIF pseudo-op?
	or	a		; NZ => IF, ELSE or ENDIF
	jr	z,S402		; don't print these if "LIST NOCOND"
;
;	OK, the conditional state is false, do we want to list
;	the false instructions?
;	CONDFL: True if we want to list false conditionals
;
S401C:	ld	a,(LSTOPT)	; bit set = print conds
	bit	CONDFL,a	; bit clear = don't print conds
	ret	z		; don't print if 'LIST NOCOND'
				;  also means IF, ELSE, ENDIF too
	ld	c,a
	ld	a,(MACFLG)
	cp	' '
	jr	z,S402
	bit	XMACFL,c	; don't print if 'LIST XMACROS' either,
	ret	nz		;  as no code was generated (this cancels the
				;   effect of the LBLFLG test above)
;
;	Print the line
;
S402:	call	LSTPRE		; prepare the line
	call	PLINE		; write and page

;-----------------------------------------------------------------
COND	LNGERR

	ld	hl,ERRFLG	; any errors?
	ld	a,' '
	cp	(hl)		; space means no
	ret	z		; so nothing else to print
	ld	(hl),a		; don't report the same error twice
	
;	Print extended error message

	ld	hl,ERRH1	; move some stars to
	ld	de,HDRBUF	;  the print buffer
	ld	bc,ERRH1L	;   to make errors
	ldir			;    conspicuous

	push	de		; save buffer pointer
	ld	a,(ERRFLG)	; get error character
	sub	'A'		; remove ASCII offset
	cp	'Z'-'A'+1	; in range?
	jr	c,S410		; skip if so
	ld	a,'X'-'A'	; else generate an error!
S410B:	add	a,a		; each entry is 2 bytes
	ld	e,a		; put index into DE
	ld	d,0
	ld	hl,ERRTAB	; start of error table
	add	hl,de		; point to msg address
	ld	e,(hl)		; get address of error msg
	inc	hl
	ld	d,(hl)
	pop	hl		; get REC ptr back
ERRMLP:	ld	a,(de)		; get a char from the msg
	or	a
	jr	z,ERRMDN
	ld	(hl),a		; store in print buffer
	inc	hl
	inc	de
	jp	ERRMLP

ERRMDN:	ex	de,hl		; DE points to output buffer
	ld	hl,ERRH2	; some more stars
	ld	bc,ERRH2L
	ldir
	call	PLINE		; output error message

ENDC
;-----------------------------------------------------------------

COND	1-LNGERR

	ld	a,' '
	ld	(ERRFLG),a	; don't report the same error twice

ENDC

	ret
;
;	Get ASCII character representing mode or segment type
;
SEGCHR:	ld	a,(EQUFLG)
	or	a
	ld	a,(EVMODE)
	jr	nz,SEGCH
	ld	a,(CURSEG)
SEGCH:	and	.not. GBLSYM	; strip PUBLIC bit
	cp	40h		; TEXT
	ld	c,27H	;'
	ret	z
	cp	80h		; DATA
	ld	c,'"'
	ret	z
	cp	0C0h		; BSS
	ld	c,'!'
	ret	z
	and	EXTSYM
	ld	c,'#'
	ret	nz
	ld	c,' '
	ret
;
;	Output byte value to listing, A = value
;
LSTB:	ld	e,a
	ld	a,(LSTCNT)
	ld	c,a
	add	a,3
	cp	HDRSZ-12+1	; value fits in field?
	jr	c,LB1		; branch if yes
	push	de		; else overflow to next line
	call	LSTOUT		; print the line
	call	LSTINI		; prepare for next
	pop	de
	ld	a,3
	ld	c,0
LB1:	ld	(LSTCNT),a
	ld	b,0
	ld	hl,HDRBUF+8
	add	hl,bc
	ld	a,e
	jp	CNV2HX		; convert and store byte value
;
;	Output word value to listing, HL = value, C = mode
;
LSTW:	ld	a,(LSTCNT)
	ld	b,a
	add	a,6
	cp	HDRSZ-12+1	; word fits in field?
	jr	c,LW1		; branch if yes
	push	hl		; else overflow to next line
	push	bc
	call	LSTOUT		; print the line
	call	LSTINI		; prepare for next
	pop	bc
	pop	hl
	ld	a,6
	ld	b,0
LW1:	ld	(LSTCNT),a
	ld	a,c
	ld	c,b
	ld	b,0
	ex	de,hl
	ld	hl,HDRBUF+8
	add	hl,bc
	ld	c,a
	ld	a,d
	call	CNV2HX		; convert and store word value
	ld	a,e
	call	CNV2HX
	ld	a,c
	call	SEGCH
	ld	(hl),c		; store mode indicator
	ret
;
;	PLINE routine - Write and Page
;
PLINE:	ld	hl,MAXLNE
	ld	a,(CURLNE)
	cp	(hl)		; page full?
	jr	c,PLINE1	; branch if not
;
;	Headers are required if we generate a listing
;	to any destination. Suppressed if 'Z' option.
;
	xor	a
	ld	(CURLNE),a

	ld	a,(LFLAG)	; get list flag
	cp	'Z'		; Z = no print
	jr	z,PLINE1	; skip heading

	ld	a,(ERRFLG)
	push	af
	ld	a,' '
	ld	(ERRFLG),a
	ld	hl,HOFPG+3	; increment page number
	call	INCNUM
	ld	hl,HOFMSG	; print it
	call	PLINE2		; (don't inc line no)
	ld	hl,TITLEB	; print title
	ld	a,(hl)
	or	a
	call	nz,PLINE2
	ld	hl,SBTTLB	; print subtitle
	ld	a,(hl)
	or	a
	call	nz,PLINE2
	ld	hl,HOFEND
	call	PLINE2		; newline
	pop	af
	ld	(ERRFLG),a

PLINE1:	ld	hl,HDRBUF	; point to buffer

PLINE2:	ld	a,(CURLNE)
	inc	a		; increase page line counter
	ld	(CURLNE),a

	ld	a,(LFLAG)	; get list flag
	cp	'Z'		; Z = no print, errors to console
	jr	z,PLINE3	; skip print, but send errors
	cp	'X'		; list to console only
	jp	z,CLINE
	cp	'Y'		; errors to printer?
	jr	nz,PLINE3
	push	af		; save list flag
	push	hl		; save ptr line buffer
	call	CLINE		; listing to console
	pop	hl		; recover line buff ptr
	pop	af		; get list flag
	ld	a,(ERRFLG)	; fetch error flag
	cp	' '		; blank?
	jp	nz,WLINE	; errors to printer
	ret			; else done
;
PLINE3:	ld	a,(ERRFLG)	; fetch error flag
	cp	' '		; blank?
	push	hl		; save ptr line buffer
	call	nz,CLINE	; listing to console
	pop	hl		; recover line buff ptr
	ld	a,(LFLAG)	; get list flag
	cp	'P'		; listing to printer?
	jp	z,WLINE		; listing to printer
	cp	'Z'		; listing supressed?
	ret	z		; done if yes
				; else listing to disk .PRN file
	push	hl		; save line buffer ptr
PRNOPT:	ld	a,(hl)		; fetch char from line buffer
	or	a		; is char terminator?
	jr	z,PRCRLF	; exit loop if yes
	call	WNB		; write char to print buffer
	inc	hl		; incr line ptr
	jr	PRNOPT		; loop, more on line
PRCRLF:	ld	a,CR		; end with a newline
	call	WNB
	ld	a,LF
	call	WNB
	pop	hl		; recover buffer ptr
	ret			; done
;
;	CNV2HX - Convert contents of A reg to hex characters
;	and place in buffer pointed to by HL
;
CNV2HX:	push	af
	rrca
	rrca
	rrca
	rrca
	call	CNV2H
	pop	af
CNV2H:	and	0Fh
	add	a,90h
	daa
	adc	a,40h
	daa
	ld	(hl),a
	inc	hl
	ret
;
;	INCNUM - Increment ASCII number
;
INCNUM:	ld	a,(hl)
	cp	' '
	jr	nz,INCNU1
	ld	a,'0'
INCNU1:	inc	a
	ld	(hl),a
	cp	'9'+1
	ret	nz
	ld	(hl),'0'
	dec	hl
	jr	INCNUM

;	SUBTTL	'Macro Table sort and list'
;
;	Sort macro table and list it. The routine is called only
;	if at least one macro has been defined.
;
;	Build macro pointer table, a list of addresses
;	pointing to the start of each macro definition entry
;
MACROS:	ld	hl,HDRMAC
	ld	bc,HMSIZE
	call	PMSG		; output header

	ld	hl,(SYMPTR)	; get address of next free memory space
	inc	hl		; point to byte after
	ld	(SYMREF),hl	; start of macro ref table
	ld	(MACPTR),hl	; reuse MACPTR as ref table pointer

	ld	c,STMDEF
	call	FNDREC
	ld	bc,0		; BC = symbol counter
LMAC1:	inc	bc
	push	bc
	push	hl
	inc	hl
	inc	hl
	inc	hl
	ex	de,hl
	ld	bc,2
	call	MACCHK		; check memory
	jr	c,LMAC5		; jp if overflow
	ld	hl,(MACPTR)	; pointer into reference table
	ld	(hl),e		; store macro start address
	inc	hl		; bump pointer
	ld	(hl),d		; high byte of address
	inc	hl		; bump pointer
	ld	(MACPTR),hl	; and save for later
	pop	hl
	ld	c,STMDEF
	call	FNDNXT
	pop	bc
	jr	nc,LMAC1

LMAC2:	push	bc
	ld	a,(LSTOPT)
	bit	SORTFL,a	; do we want sorted symbol table?
	call	nz,SORT		; sort pointers if yes
	pop	bc

	ld	hl,(SYMREF)	; start of macro table pointers
	ld	(MACPTR),hl	; save pointer
LMAC3:	ld	a,' '
	ld	hl,HDRBUF
	ld	de,HDRBUF+1
	ld	(hl),a
	push	bc
	ld	bc,HDRSZ+RECMAX-1
	ldir
	pop	bc
	xor	a		; set up symbols-per-line counter
	ld	(SYMCNT),a

LMAC4:	ld	hl,(MACPTR)	; get macro pointer back
	ld	e,(hl)		; get pointer
	inc	hl
	ld	d,(hl)		; hi byte
	inc	hl
	ld	(MACPTR),hl	; save reference pointer
	push	de		; save macro pointer

	ld	hl,HDRBUF	; point to output buffer
	ld	a,(SYMCNT)	; which symbol?
;	ld	e,a		; multiply by 42 (IDMAX+11) ;26
				; 42 = 32 + 8 + 2
	add	a,a		; *2
	ld	e,a		; E=*2
	add	a,a		; *4
	add	a,a		; *8
	ld	d,a		; D=*8
	add	a,a		; *16
	add	a,a		; *32
	add	a,d
	add	a,e		; *(32+8+2)

;	add	a,a		; *2
;	ld	d,a
;	add	a,e		; *3
;	add	a,a		; *6
;	add	a,a		; *12
;	add	a,a		; *24
;	add	a,d		; *26

	ld	e,a
	ld	d,0
	add	hl,de		; index into line
	ex	de,hl		; put output record pointer in DE
	pop	hl		; get macro pointer back
	ld	a,b
	or	c		; test for end of table
	jr	z,MACXIT
	dec	bc
	push	bc
	ld	c,(hl)		; length in BC
	ld	b,0
	inc	hl		; point to start of macro name
	ldir			; copy name
	ld	a,(SYMCNT)
	inc	a
	ld	(SYMCNT),a
	cp	3		; 4 for wide listing
	pop	bc
	jr	c,LMAC4

	push	bc
	ex	de,hl
	ld	(hl),0		; terminate the line
	call	PLINE
	pop	bc
	jr	LMAC3

LMAC5:	pop	hl
	pop	bc
	jp	WERROR

MACXIT:	ex	de,hl		; HL = buffer pointer
	ld	(hl),0		; terminate the line
	call	PLINE		; print it
	ld	hl,HDRBUF
	ld	(hl),0
	jp	PLINE		; print an extra blank line and return

HDRMAC:	defm	'Macros:'
	defb	0
HMSIZE	equ	$-HDRMAC

PMSG:	ld	de,HDRBUF
	ldir
	jp	PLINE

;	SUBTTL	'Symbol Table sort and list'
;
;	Sort symbol table and list it
;
;	Build symbol pointer table, a list of addresses
;	pointing to the start of each symbol table entry
;
SYMBOL:	ld	hl,HDRSYM
	ld	bc,HSSIZE
	call	PMSG		; output header
	ld	hl,(SYMPTR)	; get address of next free memory space
	inc	hl		; point to byte after
	ld	(SYMREF),hl	; start of symbol ref table
	ld	(MACPTR),hl	; ref table pointer

	ld	de,(SYMTBL)	; start of symbols
	ld	bc,0		; BC = symbol counter
SYMB5:	ld	a,(de)		; get length of this symbol
	or	a
	jr	z,SYMB6		; finish if done
	inc	bc
	push	bc
	ld	bc,2
	call	MACCHK		; check memory
	pop	bc
	jp	c,WERROR	; jp if overflow
	ld	hl,(MACPTR)	; pointer into reference table
	ld	(hl),e		; store symbol start address
	inc	hl		; bump pointer
	ld	(hl),d		; high byte of address
	inc	hl		; bump pointer
	ld	(MACPTR),hl	; and save for later
	ld	a,(de)		; get length of this symbol
	and	1Fh		; in bottom 5 bits ; 0Fh
	add	a,4		; 4 for overhead ;6
	ld	l,a		; add to start address
	ld	h,0		; to find start of next symbol
	add	hl,de		; HL = start of next symbol
	ex	de,hl		; DE = start of next symbol
	jr	SYMB5		; round again

SYMB6:	push	bc
	ld	a,(LSTOPT)
	bit	SORTFL,a	; do we want sorted symbol table?
	call	nz,SORT		; sort pointers if yes
	pop	bc

	ld	hl,(SYMREF)	; start of symbol table pointers
	ld	(MACPTR),hl	; save pointer
SYMB65:	ld	a,' '
	ld	hl,HDRBUF
	ld	de,HDRBUF+1
	ld	(hl),a
	push	bc
	ld	bc,HDRSZ+RECMAX-1
	ldir
	pop	bc
	xor	a		; set up symbols-per-line counter
	ld	(SYMCNT),a

SYMB70:	ld	hl,(MACPTR)	; get symbol pointer back
	ld	e,(hl)		; get pointer
	inc	hl
	ld	d,(hl)		; hi byte
	inc	hl
	ld	(MACPTR),hl	; save reference pointer
	push	de		; save symbol pointer

	ld	hl,HDRBUF	; point to output buffer
	ld	a,(SYMCNT)	; which symbol?
;	ld	e,a		; multiply by 42 (IDMAX+11) ;26
				; 42 = 32 + 8 + 2
	add	a,a		; *2
	ld	e,a		; E=*2
	add	a,a		; *4
	add	a,a		; *8
	ld	d,a		; D=*8
	add	a,a		; *16
	add	a,a		; *32
	add	a,d
	add	a,e		; *(32+8+2)

;	add	a,a		; *2
;	ld	d,a
;	add	a,e		; *3
;	add	a,a		; *6
;	add	a,a		; *12
;	add	a,a		; *24
;	add	a,d		; *26

	ld	e,a
	ld	d,0
	add	hl,de		; index into line
	ex	de,hl		; put output record pointer in DE
	pop	hl		; get symbol pointer back
	ld	a,b
	or	c		; test for end of table
	jr	z,SYMXIT
	dec	bc
	push	bc
;  IF 0
;	push	de		; save record pointer
;	ld	a,(hl)		; get length
;	and	0Fh
;	ld	c,a		; length in BC
;	ld	b,0
;	inc	hl		; point to start of symbol
;	ldir			; copy name
;	ld	c,(hl)		; BC = value
;	inc	hl
;	ld	b,(hl)
;	inc	hl
;	ld	a,(hl)		; A = type
;	pop	hl		; get record pointer
;	ld	de,17		; move over to value (IDMAX+2)
;	add	hl,de
;	ld	(hl),'='
;	inc	hl
; ELSE
	ld	a,(hl)		; get length
	and	1Fh		; 0Fh
	ld	c,a		; length in BC
	ld	b,0
	inc	hl		; point to start of symbol
	ldir			; copy name
	ld	c,(hl)		; BC = value
	inc	hl
	ld	b,(hl)
	inc	hl
	ld	l,(hl)		; A = type
	ex	de,hl
	inc	hl
	sub	33		; IDMAX(31) + 2	;17
SYMB73:	ld	(hl),'.'
	inc	hl
	inc	a
	jr	nz,SYMB73
	ld	a,e
;  ENDIF
	inc	hl
	ld	e,a		; save type
	and	EXTSYM		; External?
	jr	z,SYMB71	; branch if not
	ld	bc,0		; else display value as 0000
SYMB71:	ld	a,b
	call	CNV2HX
	ld	a,c
	call	CNV2HX

	ld	a,e		; get type
	and	0C0h+EXTSYM	; mask segment type and External bits
	call	SEGCH
	ld	(hl),c
	inc	hl

	ld	a,e		; get type
	and	GBLSYM		; Public?
	jr	z,SYMB72
	ld	(hl),'P'
	inc	hl

SYMB72:	ld	a,(SYMCNT)
	inc	a
	ld	(SYMCNT),a
	cp	3		; 4 for wide listing
	pop	bc
	jr	c,SYMB70

	push	bc
	ld	(hl),0		; terminate the line
	call	PLINE
	pop	bc
	jp	SYMB65

SYMXIT:	ex	de,hl		; HL = buffer pointer
	ld	(hl),0		; terminate the line
	jp	PLINE		; print it and return

HDRSYM:	defm	'Symbols:'
	defb	0
HSSIZE	equ	$-HDRSYM

;
;	Sort the pointers using a simple bubble sort comparing the
;	symbols.
;
SORT:	ld	a,b
	or	c
	ret	z		; no symbols to sort, return
SORT1:	dec	bc
	ld	a,b
	or	c
	ret	z		; only one symbol, return
	push	bc
	inc	bc
	xor	a		; cheap zero
	ld	(SWPFLG),a	; initialise the swap flag
	ld	hl,(SYMREF)	; start of symbol table pointers
SORT2:	ld	e,(hl)		; get first pointer
	inc	hl
	ld	d,(hl)		; hi byte
	inc	hl
	push	hl		; save pointer
	ld	a,(hl)		; get 2nd pointer
	inc	hl
	ld	h,(hl)		; hi byte
	ld	l,a		; HL = 2nd pointer
	dec	bc
	ld	a,b
	or	c
	jr	z,SORT7		; end of this pass
	push	bc
	ld	a,(de)		; get length of first symbol
	and	1Fh		; 5 bits only ;4
	ld	b,a		; save in B
	ld	a,(hl)		; get length of 2nd symbol
	and	1Fh		; 5 bits only ;4
	ld	c,a		; save in C for later
	push	hl		; save all pointers
	push	de		; and lengths
	push	bc
	cp	b		; compare to length of first
	jr	nc,SORT3	; skip if 1st shorter
	ld	b,a		; 2nd is shorter
SORT3:	inc	de		; point into
	inc	hl		;  actual label
	ld	a,(de)		; get char from 1st
	cp	(hl)		; compare to 2nd
	jr	c,SORT5		; 2nd > 1st so no swap
	jr	nz,SORT4	; 1st > 2nd so swap immediate
	djnz	SORT3		; dec character count and loop for next char
;
;	By now the symbols are the same (as far as we have checked.)
;	Now swap pointers only if 2nd is longer than 1st.
;
	pop	bc		; get lengths back
	ld	a,c		; get 2nd length
	cp	b		; is 1st longer?
	jr	nc,SORT6	; don't swap if not
	dec	sp		; clean up stack
	dec	sp		; to just drop through
;
;	1st > 2nd, swap pointers
;
SORT4:	pop	bc		; get counts back & ignore
	pop	de		; pop first pointer into BE
	pop	bc		; pop second pointer into BC
	pop	hl
	ex	(sp),hl		; get reference pointer
	dec	hl		; drop back to first pointer
	dec	hl
	ld	(hl),c		; and store 2nd pointer there
	inc	hl
	ld	(hl),b
	inc	hl
	ld	(hl),e		; store first pointer
	inc	hl
	ld	(hl),d
	dec	hl		; restore ref pointer
	ld	a,0FFh		; set the swap flag
	ld	(SWPFLG),a	;  to say we have swapped
	pop	bc
	jr	SORT2		; and round again
;
;	2nd > 1st, no swap required
;
SORT5:	pop	bc		; get counts back
SORT6:	pop	de		; pop first pointer
	pop	hl		; pop second pointer
	pop	bc		; get symbol counter
	pop	hl		; get ref pointer
	jr	SORT2		; and round again
;
SORT7:	pop	hl
	pop	bc
	ld	a,(SWPFLG)
	or	a
	jr	nz,SORT1	; round again
	ret			; sort complete, return

;	IF	LNGERR
;
;	SUBTTL	'Error Messages'
;
;ERRH1:	db	'*****  '
;ERRH1L	equ	$-ERRH1
;ERRH2:	db	'  *****',0
;ERRH2L	equ	$-ERRH2
;
;	Error message storage
;
;	Index table
;
;ERRTAB:	dw	ERRMA
;	dw	ERRMB
;	dw	ERRMC
;	dw	ERRMD
;	dw	ERRME
;	dw	ERRMF
;	dw	ERRMG
;	dw	ERRMH
;	dw	ERRMI
;	dw	ERRMJ
;	dw	ERRMK
;	dw	ERRML
;	dw	ERRMM
;	dw	ERRMN
;	dw	ERRMO
;	dw	ERRMP
;	dw	ERRMQ
;	dw	ERRMR
;	dw	ERRMS
;	dw	ERRMT
;	dw	ERRMU
;	dw	ERRMV
;	dw	ERRMW
;	dw	ERRMX
;	dw	ERRMY
;	dw	ERRMZ
;
;	Actual error messages
;
;ERRMA:	db	'Too many IF statements',0
;ERRMB:	db	'ENDIF without matching IF statement',0
;ERRMC:	db	'ELSE without matching IF statement',0
;ERRMD:	db	'Relative jump range error',0
;ERRME:	db	'Expression error',0
;ERRMF:	db	0
;ERRMG:	db	0
;ERRMH:	db	0
;ERRMI:	db	0
;ERRMJ:	db	0
;ERRMK:	db	0
;ERRML:	db	'Invalid identifier',0
;ERRMM:	db	'Multiple definition',0
;ERRMN:	db	'Illegal opcode',0
;ERRMO:	db	'Syntax error',0
;ERRMP:	db	'Phase error',0
;ERRMQ:	db	'Missing closing quote',0
;ERRMR:	db	'Relocation error',0
;ERRMS:	db	0
;ERRMT:	db	'Missing ENDM or ENDIF',0
;ERRMU:	db	'Undefined symbol',0
;ERRMV:	db	'Value error',0
;ERRMW:	db	'Symbol table overflow',0
;ERRMX:	db	'Unknown error',0
;ERRMY:	db	0
;ERRMZ:	db	'Divide by Zero',0

;	ENDIF

	psect	data

MAXLNE:	defb	0	; lines per page
LSTCNT:	defb	0	; object code field character count
PCLINE:	defw	0	; PC for current listing line
SYMREF:	defw	0	; symbol ref address
SWPFLG:	defb	0	; flag to indicate a swap in the sort
SYMCNT:	defb	0	; symbols-per-line counter

;	END
