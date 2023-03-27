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

;	TITLE	Z80AS Macro-Assembler
;	SUBTTL	MACROS support

*include	ZSM.INC

	GLOBAL	ID,ADDSYM

; Macro types

$MACRO	equ	1
$REPT	equ	2
$IRP	equ	3
$IRPC	equ	4

;public
	global	DEFMAC,ENDMAC,MSTORE,MDFLVL,MACLVL,DFREPT
	global	DFIRP,DFIRPC,GNRMAC,FNDMAC,EXPMAC,GETSTR
	global	MACPTR,CVTNUM,LCLNUM
;extern
	global	ID,REC,SYMPTR,NEWSYM,DSPTR,IDADR,IDBUF,IDLEN
	global	GNC,ERRFLG,PTR1,FNDOPC,EVALNEW,RELERR,OERROR
	global	VALID,BACKUP,EVMODE,WERROR,FNDREC,FNDNXT,MFREE
	global	CNV2HX,MCHECK,MACCHK,ERRQ,GNR,RADIX,SYMLUK
	global	SYMTBL,GBCOL,CNDSTK,CONDSP,CLEVEL

	psect	text

;	SUBTTL	'MACRO pseudo-operator'
;
;	Macro structure as saved in high memory:
;
;  DSPTR:
;	db	STMDEF		; type = MACRO def
;	dw	len		; length of following block:
;	db	len,'name'	;  macro name
;	db	nlocal		;  number of local variables
;	db	'body line 1',0	;  macro body, dummy params replaced by
;	db	'body line 2',0	;   80h + param number, local vars by
;	db	'body line 3',0	;    0C0h + var number
;	db	...		;
;	db	EOF		;  end of macro marker
;
;	Macros are built first on low memory (following symbol table)
;	and moved to upper memory when the final size is known.
;
;  SYMPTR:
;	db	len,'name'	; macro name (old var name)
;	db	len,'arg1'	; dummy param list, needed only for scanning
;	db	len,'arg2'	;  of macro definition body and will not be
;	db	len,'arg3'	;   copied to high mem, len has the hi-bit
;	db	len,'arg4'	;    set if it corresponds to a local var
;	db	...		;
;	db	0FFh		; end of args
;	db	nlocal		; number of local variables
;	db	'body line 1',0	; params and local variables replaced by
;	db	'body line 2',0	;  single-byte code as explained above
;	db	'body line 3',0	;
;	db	...		;
;	db	EOF		; end of macro marker
;
; Nested macros are allowed. However, nesting happens at execution time and
; not at definition time (i.e. an inner macro becomes defined only when the
; outer macro is executed). The macro generation code has to track the
; nesting level nevertheless to prevent exiting prematurely the macro
; definition mode when the ENDM of an inner macro is encountered.
;

DEFMAC:	call	ID		;get macro name (ZAS type syntax)
	call	ADDSYM		;add-it as symbol
	ld	hl,(IDADR)	; get pointer to identifier (MACRO name)
	ld	a,h
	or	l
	jp	z,OERROR	; ensure is present

	ld	a,(NEWSYM)
	or	a
	jr	nz,NEWMAC	; branch if new symbol

	; M80 allows macros to have the same name as existing symbols,
	; or even existing macros. Here we do the same.

	; Macros have priority over instruction mnemonics.

	ld	a,(hl)		; get identity char
	and	1Fh		;0Fh
	jp	z,OERROR	; error if name is invalid
	ld	c,a
	ld	b,0
	inc	bc
	ld	de,(SYMPTR)	; for code @NEWMAC
	ld	(IDADR),de
	ldir			; copy length + name

NEWMAC:	ld	hl,(IDADR)
	ld	(SYMPTR),hl	; remove new symbol from symbol table

	ld	a,(hl)		; get length+flags
	and	1Fh		;0Fh ; remove flags, leave length
	ld	(hl),a

	push	hl
	ld	(IDLEN),a
	inc	hl
	ld	de,IDBUF
	ld	c,a
	ld	b,0
	ldir
	call	FNDMAC		; is this a macro re-definition?
	jr	c,NM1		; branch if not
	ld	a,STDEL
	ld	(de),a		; else delete old macro
	ld	a,(MACLVL)
	or	a
	call	z,GBCOL		; if at safe level, call garbage collector
NM1:	ld	a,(IDLEN)
	pop	hl

	ld	c,a
	ld	b,0
	inc	bc
	inc	bc
	call	MCHECK		; make sure we have enough memory
	dec	bc
	jp	c,WERROR
	add	hl,bc
	ld	(MACPTR),hl	; set macro build pointer
	ld	(PARAMS),hl	; set pointer to parameters

	xor	a
	ld	(LCLFLG),a	; clear LOCAL flag for GETPRM

	call	GNC		; get next character
	or	a		; end of line?
	call	nz,GETPRM	; get parameters if not

	xor	a
	ld	(LCOUNT),a	; reset LOCAL variable count

	ld	hl,(MACPTR)
	ld	(hl),0FFh	; set end of args marker
	inc	hl
	ld	(MACPTR),hl

	ld	a,$MACRO
	ld	(MACTYP),a	; remember type is MACRO

INCDFL:	ld	hl,MDFLVL
	inc	(hl)		; increase macro def level
	ret	nz
	dec	(hl)
VERROR:	ld	a,'V'		; on overflow, set error flag
	ld	(ERRFLG),a
	ret
;
;	Get MACRO dummy parameters or LOCAL variables
;
GETPRM:	ld	bc,RECMAX	; ensure enough available memory
	call	MACCHK		;  (RECMAX is a bit more than enough here,
	jp	c,WERROR	;   but that will save time and some bytes)
	call	BACKUP
GP0:	call	ID		; get identifier
	ld	a,(IDLEN)
	or	a		; valid identifier?
	jr	nz,GP1		; yes -> branch
	call	OERROR		; no -> argument error
	jr	GP2
GP1:	ld	c,a
	ld	b,0
	ld	hl,IDBUF
	ld	de,(MACPTR)
	ld	a,(LCLFLG)
	or	c		; set MSB of length if LOCAL variable name
	ld	(de),a		; store length
	inc	de
	ldir
	ld	(MACPTR),de
	ld	hl,LCOUNT
	inc	(hl)		; increase LOCAL variable count
GP2:	call	GNC
	cp	','		; more params?
	jr	z,GP0		; loop if yes
	or	a		; end of line?
	ret	z		; return Z if yes
	jp	BACKUP		; else backup to scanned char and return
;
;	Store macro, called from the main thread while in macro def mode.
;
MSTORE:	ld	hl,(PTR1)
	push	hl		; save current PTR1
	call	ID		; get first identifier
	cp	':'
	jr	z,MST1		; skip if label
	cp	'&'		; test for possible param substitution
	jr	nz,MST0
	ld	hl,(PTR1)
MSKIP:	ld	a,(hl)		; if yes, ignore everything
	or	a		;  up to the next delimiter
	jr	z,MSKP1
	cp	' '
	jr	z,MSKP1
	cp	TAB
	jr	z,MSKP1
	inc	hl
	cp	':'
	jr	z,MSKP1
	jr	MSKIP
MSKP1:	ld	(PTR1),hl
	jr	MST2
MST0:	call	FNDOPC		; lookup opcode
	jr	nc,MST3		; branch if found
	jr	MST2
MST1:	call	GNC		; skip over delimiter
MST2:	call	ID		; try second identifier
	call	FNDOPC		; lookup opcode
	jp	c,MST8		; branch if not found
MST3:	ld	e,(hl)
	inc	hl
	ld	d,(hl)
	inc	hl
	ld	a,(hl)
	cp	27		; class 27? (pseudo-ops)
	jp	nz,MST8		; branch if not
	ld	a,e
	cp	24		; MACRO/REPT/IRP/IRPC?
	jp	z,MST7		; branch if yes - nested macro definition
	cp	25		; ENDM?
	jr	z,MST4		; branch if yes
	cp	27		; LOCAL?
	jp	nz,MST8		; branch if not

	; LOCAL

	ld	a,(MACTYP)
	cp	$MACRO
	jp	nz,MST8		; only MACROs can have LOCALs

	ld	a,(MDFLVL)
	dec	a
	jp	nz,MST8		; don't process it if inside a nested macro

	pop	hl		; drop saved PTR1
	ld	hl,(MACPTR)
	dec	hl
	ld	a,(hl)
	inc	a		; test for 0FFh
	ret	nz		; too late, LOCAL not allowed here

	ld	a,80h
	ld	(LCLFLG),a	; set LOCAL flag for GETPRM

	ld	(MACPTR),hl
	call	GNC		; get next character
	or	a		; end of line?
	jp	z,OERROR	; error if yes
	call	GETPRM		; else get parameter list

	ld	hl,(MACPTR)
	ld	(hl),0FFh	; set new end of args
	inc	hl
	ld	(MACPTR),hl
	ret

MST4:	ld	hl,MDFLVL
	dec	(hl)		; decrement macro level
	jp	nz,MST8

	; ENDM

	ld	a,(MACLVL)
	or	a
	call	z,GBCOL		; if at safe level, call garbage collector

	pop	hl		; drop saved PTR1
	ld	hl,(MACPTR)
	ld	(hl),EOF	; set end of macro marker
	inc	hl
	ld	(MACPTR),hl

	ld	a,(MACTYP)	; check macro type
	cp	$MACRO
	jr	nz,MST6		; branch if REPT/IRP/IRPC - execute immediately

	; end MACRO definition

	ld	de,(SYMPTR)	; begin of macro build area
MST5:	ld	a,(de)
	inc	de
	inc	a		; 0FFh marks end of param list
	jr	nz,MST5		; find begin of macro body (use CPIR?)

	ld	hl,(MACPTR)
	push	hl
	or	a
	sbc	hl,de		; obtain length
	ld	c,l
	ld	b,h
	ld	de,(DSPTR)
	pop	hl
	push	bc
	dec	hl
	dec	de
	lddr			; copy to high memory

	ld	a,(LCOUNT)
	ld	(de),a		; save number of LOCAL variables
	dec	de

	ld	hl,(SYMPTR)
	ld	a,(hl)		; get length
	ld	c,a
	ld	b,0
	inc	bc
	push	bc
	add	hl,bc
	dec	hl
	lddr			; copy name + len

	pop	hl
	pop	bc
	add	hl,bc
	inc	hl		; account for LOCAL variable count field

	ex	de,hl
	ld	(hl),d
	dec	hl
	ld	(hl),e		; set length
	dec	hl
	ld	(hl),STMDEF	; struct type = macro def
	ld	(DSPTR),hl
	ld	hl,(SYMPTR)
	ld	(hl),0		; restore end of symbol table marker
	ret

MST6:	; REPT/IRP/IRPC execution

	ld	de,(MACBGN)
	ld	hl,(MACPTR)
	push	hl
	or	a
	sbc	hl,de		; obtain length
	ld	c,l
	ld	b,h
	ld	de,(DSPTR)
	pop	hl
	push	bc
	dec	hl
	dec	de
	lddr			; copy to high memory

	pop	bc
	ex	de,hl
	xor	a
	ld	(hl),a		; clear text index
	dec	hl
	ld	(hl),a
	dec	hl
	ld	a,(CLEVEL)
	ld	(hl),a		; store current conditional level
	dec	hl
	ld	a,(MACTYP)
	ld	(hl),a		; set macro type
	dec	hl
	inc	bc
	inc	bc
	inc	bc
	inc	bc
	ld	(hl),b
	dec	hl
	ld	(hl),c		; set length
	dec	hl
	ld	(hl),STMEXP	; struct type = macro expansion
	ld	(DSPTR),hl
	ld	hl,(SYMPTR)
	ld	(hl),0		; restore end of symbol table marker
	ld	hl,MACLVL
	inc	(hl)		; start macro execution
	ret	nz
	dec	(hl)
	jp	VERROR

MST7:	call	INCDFL
MST8:	ld	a,' '
	ld	(ERRFLG),a
	pop	hl
	ld	(PTR1),hl	; restore old PTR1

	ld	bc,0
MST9:	ld	a,(hl)
	inc	hl
	inc	bc		; compute length, including trailing null
	or	a
	jr	nz,MST9

	call	MACCHK		; check for enough space
	jp	c,WERROR	; error if out of memory

	ld	hl,(PTR1)
	ld	de,(MACPTR)
	push	de
	ldir			; copy line to macro body
	ld	(MACPTR),de
	dec	hl		; point back to trailing null
	ex	(sp),hl
	ld	(PTR1),hl
	ld	a,(ERRFLG)	; save ERRFLG, as MSCAN may generate spurious
	push	af		;  errors when testing for valid identifiers
	call	MSCAN		; scan line and encode params
	pop	af
	ld	(ERRFLG),a	; restore original ERRFLG
	xor	a
	ld	(de),a		; end line with a null
	inc	de
	ld	(MACPTR),de
	pop	hl
	ld	(PTR1),hl	; restore old record pointer
	ret
;
;	MSCAN - Scan line for dummy parameters and local variables,
;	and replace them with one-byte codes.
;
;	Code for dummy parameter = 80h + param ordinal
;	Code for local variable  = C0h + label ordinal
;
MSCAN:	ld	e,l		; get dest address in DE
	ld	d,h		;  (will overwrite src contents)
S0:	ld	a,(hl)		; get char from line
	or	a
	ret	z		; if end of line, return
	cp	';'
	jr	z,S14		; branch if start of comment
	cp	27H		;'
	jr	z,S9		; branch if start of quoted string
	cp	'"'		;  (inside a string any parameters
	jr	z,S9		;   must be preceded by an ampersand)
	cp	' '
	jr	z,S1		; store blanks directly
	cp	TAB
	jr	z,S1
S1A:	call	SUBST		; test for parameter and substitute if found
	jr	S0		; loop

S1:	ld	(de),a		; store char
	inc	hl		; advance pointers
	inc	de
	jr	S0		; loop

S9:	; quoted string

	ld	(de),a		; store quote char
	inc	de
	ld	c,a
S10:	inc	hl
S11:	ld	a,(hl)		; get next char
	or	a
	ret	z		; if end of line, return 
	cp	c		; quote?
	jr	z,S1		; exit this loop if yes
	cp	'&'		; ampersand?
	jr	nz,S12		; branch if not
	push	bc
	call	SUBST		; else match parameter
	pop	bc
	jr	S11		; loop

S12:	ld	(de),a		; store char
	inc	de		; advance pointer
	jr	S10		;  and loop

S14:	; comment

	inc	hl
	cp	(hl)		; do not store comments that begin with
	ret	z		;  two semicolons
	dec	hl
S15:	ld	(de),a		; store comment, but do not perform any
	or	a		;  parameter substitutions
	ret	z
	inc	de
	inc	hl
	ld	a,(hl)
	jr	S15
;
;	SUBST - Test for dummy parameter or local variable name
;	and substitute with the corresponding value if found.
;
SUBST:	push	af
	cp	'&'		; ampersand?
	jr	nz,S5A
	inc	hl		; skip it if yes (but remember it)
S5A:	push	hl
	ld	(PTR1),hl	; remember start of possible identifier
	call	ID
	ld	a,(IDLEN)
	or	a		; valid identifier?
	jr	z,S2		; branch if not

	push	de
	ld	c,a		; C = identifier length
	ld	b,80h		; B = parameter code
	ld	hl,(PARAMS)	; HL = dummy parameter list
S5:	ld	a,(hl)		; get dummy parameter length
	inc	a		; test for 0FFh
	jr	z,S3		; parameter not found, exit loop
	dec	a
	bit	7,a		; hi-bit set?
	jr	z,S8		; branch if not
	bit	6,b		; first time?
	jr	nz,S8		; branch if not
	ld	b,0C0h		; else make B = LOCAL label code
S8:	and	7Fh		; kill hi-bit
	cp	c
	jr	nz,S4		; not same length
	push	hl
	push	bc
	inc	hl
	ld	b,a
	ld	de,IDBUF
S6:	ld	a,(de)
	cp	(hl)		; compare strings (both already in uppercase)
	jr	nz,S7		; not same
	inc	hl
	inc	de
	djnz	S6
	pop	bc
	pop	hl		; matches
	pop	de
	ld	a,b
	ld	(de),a		; store parameter code
	inc	de
	pop	hl
	pop	af
	ld	hl,(PTR1)	; point past identifier
	ret

S7:	pop	bc
	pop	hl
	ld	a,(hl)
	and	7Fh		; get length
S4:	ld	e,a
	ld	d,0
	add	hl,de
	inc	hl		; point to next parameter
	inc	b
	jr	S5		; loop to match next

S3:	pop	de		; parameter not found, restore registers
	pop	hl
	pop	af
	cp	'&'		; first char was ampersand?
	jr	nz,S3A		; branch if not
	ld	(de),a		; else restore it
	inc	de
S3A:	ld	b,0
	ldir			; copy identifier name
S3B:	ld	a,(hl)
	call	VALID		;  and any other following VALID chars
	ret	c
	ld	(de),a
	inc	hl
	inc	de
	jr	S3B

S2:	pop	hl		; not an identifier, restore registers
	pop	af
	cp	'&'		; first char was ampersand?
	jr	nz,S2A		; branch if not
	ld	(de),a		; else restore it
	inc	de
S2A:	ld	a,(hl)		; copy char
	ld	(de),a
	inc	hl
	inc	de
	ret

;	SUBTTL	'MACRO processing'
;
;	EXPMAC - Expand MACRO.
;	Called with HL pointing to macro body in dynamic area
;	(e.g. as returned by FNDMAC).
;
;	Macro expansion structure as saved in high memory:
;
;	db	STMEXP		; type = MACRO expansion
;	dw	len		; length of the following block:
;	db	$MACRO		;  macro type
;	db	clevel		;  saved conditional level
;	dw	index		;  text index
;	dw	local		;  first/base number for LOCAL variables
;	dw	macptr		;  pointer to macro def
;	db	len,'arg1'	;  arg 1
;	db	len,'arg2'	;  arg 2
;	db	...		;
;	db	0FFh		;  end of args
;
EXPMAC:	ld	bc,6
	call	MCHECK
	jp	c,WERROR
	ld	a,(hl)		; get number of LOCAL variables
	inc	hl		; point to start of macro text
	push	hl
	ld	bc,(LCLNUM)	; get LOCAL base number into BC
	ld	l,a
	ld	h,0
	add	hl,bc
	ld	(LCLNUM),hl	; save number of next LOCAL variable
	pop	de
	ld	hl,(SYMPTR)
	ld	(MACBGN),hl
	ld	(hl),c		; store base number of LOCAL labels
	inc	hl
	ld	(hl),b
	inc	hl
	ld	(hl),e		; store pointer to macro def
	inc	hl
	ld	(hl),d
	inc	hl
	ld	(MACPTR),hl
	call	GNC
	or	a
	call	nz,GETARG	; get arguments
	ld	(hl),0FFh	; end of args
	inc	hl
	ld	(MACPTR),hl
	ld	a,$MACRO
	ld	(MACTYP),a
	jp	MST6		; exit via common code
;
;	Get comma-separated list of arguments
;
GETARG:	ld	bc,RECMAX	; ensure enough available memory
	call	MACCHK		;  (RECMAX is a bit more than enough here,
	jp	c,WERROR	;   but will save some bytes and time)
	call	BACKUP
GT0:	ld	de,(MACPTR)
	push	de
	inc	de		; placeholder for length
	call	GETSTR		; get next argument
	ex	de,hl
	ld	(MACPTR),hl
	pop	de
	push	af
	or	a
	sbc	hl,de
	dec	hl
	ld	a,l
	ld	(de),a		; store length
	pop	af
	jr	c,GT1
	call	GNC
	or	a
	jr	z,GT1
	cp	','
	jr	z,GT0
GT1:	ld	hl,(MACPTR)
	ret
;
;	FNDMAC - Find MACRO
;
FNDMAC:	ld	c,STMDEF	; record type = macro def
	call	FNDREC
FNDM1:	ret	c		; not found
	push	hl
	inc	hl		; skip type
	inc	hl		;  and record length
	inc	hl
	ld	a,(IDLEN)
	cp	(hl)		; compare name length
	jr	nz,FNDM3	; branch if not same
	inc	hl
	ld	b,a
	ld	de,IDBUF
FNDM2:	ld	a,(de)
	cp	(hl)
	jr	nz,FNDM3
	inc	hl
	inc	de
	djnz	FNDM2
	pop	de		; found, HL now points past macro name
	ret			; return with CY clear
FNDM3:	pop	hl
	ld	c,STMDEF
	call	FNDNXT
	jr	FNDM1
;
;	Reading from MACRO/REPT/IRP/IRPC
;
GNRMAC:	ld	c,STMEXP
	call	FNDREC		; find top-level executing macro
	jr	nc,GNR1		; branch if found

	xor	a		; if not found (should not happen!)
	ld	(MACLVL),a	;  reset the macro execution level
	jp	GNR		;   and switch back to reading from file

GNR1:	inc	hl		; skip struct type
	inc	hl		;  and length fields
	inc	hl

	ld	a,(hl)		; get macro type
	inc	hl
	inc	hl		; skip over saved conditional level
	cp	$MACRO
	jr	z,XMACRO	; execute MACRO
	cp	$REPT
	jp	z,XREPT		; execute REPT
	cp	$IRP
	jp	z,XIRP		; execute IRP
	cp	$IRPC
	jp	z,XIRPC		; execute IRPC
	jp	OERROR		; should never happen
;
;	Read from MACRO and expand arguments
;
XMACRO:	push	hl		; save pointer
	ld	c,(hl)		; get char index
	inc	hl
	ld	b,(hl)
	inc	hl
	ld	e,(hl)		; get LOCAL base number
	inc	hl
	ld	d,(hl)
	ld	(LBASE),de
	inc	hl
	ld	e,(hl)		; get pointer to macro definition
	inc	hl
	ld	d,(hl)
	inc	hl
	ld	(ARGS),hl	; save pointer to arguments
	ex	de,hl
	add	hl,bc		; index into macro text
	ld	a,(hl)		; get char
	cp	EOF
	jp	z,XEND		; end of text = end of macro

	ld	de,REC
	ld	(PTR1),de
	ld	a,RECMAX-1
	ld	(RCOUNT),a
XM1:	ld	a,(hl)		; get char from macro text
	inc	hl
	inc	bc		; advance index
	or	a		; test for coded dummy parameter
	jp	p,XM3		; branch if not parameter
	push	hl		; else save text pointer
	push	bc
	ld	hl,(ARGS)	; get start address of argument list
	sub	80h-1		; get param number
	cp	40h+1
	jr	nc,XM8		; branch if local variable
	ld	b,a		; B = param number
XM6:	ld	a,(hl)		; get length
	inc	a		; 0FFh marks end of arguments
	jr	z,XM4		; branch if not found
	dec	a
	inc	hl		; point to string
	djnz	XM5		; count until found
	or	a
	jr	z,XM7		; if null argument
	ld	b,a
XM2:	ld	a,(hl)
	call	SAVREC		; expand argument
	inc	hl
	djnz	XM2
XM7:	pop	bc
	pop	hl		; restore text pointers
	jr	XM1		; continue with text

XM5:	ld	e,a		; get length into DE
	ld	d,0
	add	hl,de		; point to next argument
	jr	XM6		; loop

XM8:	sub	40h+1		; get label ordinal
	ld	e,a
	ld	d,0
	ld	hl,(LBASE)
	add	hl,de
	ex	de,hl
	push	de
	ld	a,'?'
	call	SAVREC		; create label of type '??nnnn'
	call	SAVREC
	pop	de
	ld	hl,(PTR1)
	ld	c,d
	call	XM9
	ld	c,e
	call	XM9
	ld	(PTR1),hl
	pop	bc
	pop	hl
	jr	XM1

XM9:	ld	a,(RCOUNT)
	sub	2
	ret	c
	ld	(RCOUNT),a
	ld	a,c
	jp	CNV2HX

XM4:	pop	bc
	pop	hl		; not found, restore text pointers
	jr	XM1		;  and loop (will expand as null string)

XM3:	call	SAVREC		; save char in REC buffer
	or	a		; end of line?
	jr	nz,XM1		; loop if not
	ld	hl,(PTR1)
	ld	(hl),a		; append null in case line was truncated
	ld	hl,REC
	ld	(PTR1),hl
	pop	hl
	ld	(hl),c		; update text index
	inc	hl
	ld	(hl),b
	ret

SAVREC:	push	hl
	ld	e,a
	ld	hl,RCOUNT
	ld	a,(hl)
	or	a		; limit reached?
	jr	z,ST1		; return if yes
	dec	a
	ld	(hl),a
	ld	hl,(PTR1)
	ld	(hl),e		; store char in buffer
	inc	hl
	ld	(PTR1),hl
ST1:	ld	a,e
	pop	hl
	ret
;
;	Read from REPT, no arguments to expand
;
XREPT:	push	hl
	ld	c,(hl)		; get char index
	inc	hl
	ld	b,(hl)
	inc	hl
	ld	e,(hl)		; get count
	inc	hl
	ld	d,(hl)
	ld	a,d
	or	e		; counter reached zero?
	jp	z,XEND		; if yes, end execution

	inc	hl
	add	hl,bc		; index into macro text
	ld	a,(hl)
	cp	EOF		; end of text?
	jr	z,XR3		; branch if yes, start next iteration

	ld	de,REC
XR1:	ld	a,(hl)		; no arg substitution for REPT
	inc	hl
	inc	bc
	ld	(de),a		; and so no need to check for REC buf overflow
	or	a
	jr	z,XR2
	inc	de
	jr	XR1

XR2:	pop	hl
	ld	(hl),c		; update text index
	inc	hl
	ld	(hl),b
	ret

XR3:	pop	hl		; next iteration
	push	hl
	xor	a
	ld	(hl),a		; reset text index
	inc	hl
	ld	(hl),a
	inc	hl
	ld	c,(hl)
	inc	hl
	ld	b,(hl)
	dec	bc		; decrement iteration counter
	ld	(hl),b
	dec	hl
	ld	(hl),c
	pop	hl
	jr	XREPT		; restart execution
;
;	Read from IRP, expand single argument
;
XIRP:	push	hl
	ld	c,(hl)		; get text index
	inc	hl
	ld	b,(hl)
	inc	hl
	ld	a,(hl)		; get argument index
	inc	hl		; point to argument list
	inc	a
XP1:	ld	e,(hl)		; get arg length
	inc	e
	jp	z,XEND		; arg list exhausted
	dec	a
	jr	z,XP2
	ld	d,0
	add	hl,de		; point to next argument
	jr	XP1

XP2:	ld	(ARGS),hl
XP3:	ld	a,(hl)
	inc	hl
	inc	a
	jr	nz,XP3		; find start of body text
	add	hl,bc		; index into macro text
	ld	a,(hl)		; get char
	cp	EOF		; end of text?
	jr	z,XP7		; branch if yes, start next iteration

	ld	de,REC
	ld	(PTR1),de
	ld	a,RECMAX-1
	ld	(RCOUNT),a
XP4:	ld	a,(hl)		; get char from macro text
	inc	hl
	inc	bc		; advance index
	or	a		; test for coded dummy parameter
	jp	p,XP5		; branch if not parameter
	push	hl
	push	bc
	ld	hl,(ARGS)	; else expand parameter
	ld	a,(hl)
	inc	a
	jr	z,XP6		; if no arguments
	dec	a
	jr	z,XP6		; if null argument
	ld	b,a
	inc	hl
XP9:	ld	a,(hl)
	call	SAVREC		; expand argument
	inc	hl
	djnz	XP9
XP6:	pop	bc
	pop	hl
	jr	XP4
XP5:	call	SAVREC		; save char in REC buffer
	or	a		; end of line?
	jr	nz,XP4		; loop if not
	ld	hl,(PTR1)
	ld	(hl),a		; append null in case line was truncated
	ld	hl,REC
	ld	(PTR1),hl
	pop	hl
	ld	(hl),c		; update text index
	inc	hl
	ld	(hl),b
	ret

XP7:	pop	hl		; next iteration
	push	hl
	xor	a
	ld	(hl),a		; reset text index
	inc	hl
	ld	(hl),a
	inc	hl
	inc	(hl)		; advance arg index
	pop	hl
	jr	XIRP		; restart execution
;
;	Read from IRPC, expand single argument
;
XIRPC:	push	hl		; save pointer
	ld	c,(hl)		; get text index
	inc	hl
	ld	b,(hl)
	inc	hl
	ld	a,(hl)		; get string index
	inc	hl
	cp	(hl)		; compare with string length
	jr	nc,XEND		; end if string exhausted

	push	hl
	inc	hl
	ld	e,a
	ld	d,0
	add	hl,de		; index into string
	ld	a,(hl)		; get string char
	ld	(ARGS),a	; save it
	pop	hl
	ld	e,(hl)
	ld	d,0
	inc	hl
	add	hl,de		; skip string, point to begin of body text
	add	hl,bc		; index into macro text
	ld	a,(hl)		; get char
	cp	EOF		; end of text?
	jr	z,XC4		; branch if yes, start next iteration

	ld	de,REC
XC1:	ld	a,(hl)		; get char from macro text
	inc	hl
	inc	bc		; advance index
	or	a		; test for coded dummy parameter
	jp	p,XC3		; branch if not parameter
	ld	a,(ARGS)	; else use string char
XC3:	ld	(de),a		; save char in REC buf (no need for ovfl check)
	or	a		; end of line?
	jr	z,XC2		; branch if yes
	inc	de
	jr	XC1		; else loop for more
XC2:	pop	hl
	ld	(hl),c		; update text index
	inc	hl
	ld	(hl),b
	ret

XC4:	pop	hl		; next iteration
	push	hl
	xor	a
	ld	(hl),a		; reset text index
	inc	hl
	ld	(hl),a
	inc	hl
	inc	(hl)		; advance string index
	pop	hl
	jr	XIRPC		; restart execution
;
;	End of MACRO/REPT/IRP/IRPC execution
;
XEND:	pop	hl
	dec	hl
	dec	hl
	dec	hl
	dec	hl
	dec	hl
	call	MFREE		; release storage
	ld	hl,MACLVL
	dec	(hl)		; decrease macro execution level
	call	z,GBCOL		; if zero, perform garbage collection
	jp	GNR		; switch input back to GNR

;	SUBTTL	'EXITM pseudo-operator'
;
;	EXITM
;
ENDMAC:	ld	c,STMEXP
	call	FNDREC		; find top-level executing macro
	jp	c,OERROR	; error if not found: EXITM without MACRO
	push	hl
	inc	hl
	inc	hl
	inc	hl
	inc	hl
	ld	a,(hl)		; get saved conditional level
	ld	(CLEVEL),a
	ld	hl,CNDSTK
	ld	e,a
	ld	d,0
	sbc	hl,de		; note CY is still clear
	ld	(CONDSP),hl	; restore conditional stack pointer
	pop	hl
	call	MFREE		; release storage
	ld	hl,MACLVL
	dec	(hl)		; decrement macro execution level
	call	z,GBCOL
	ret

;	SUBTTL	'REPT pseudo-operator'
;
;	REPT structure as saved in high memory:
;
;	db	STMEXP		; type = MACRO expansion
;	dw	len		; length of following block:
;	db	$REPT		;  macro type
;	db	clevel		;  saved conditional level
;	dw	index		;  text index
;	dw	count		;  REPT argument
;	db	'body line 1',0	;  macro
;	db	'body line 2',0	;   body
;	db	...		;
;	db	EOF		;  end of macro
;
DFREPT:	ld	bc,2
	call	MCHECK
	jp	c,WERROR
	call	EVALNEW		; get argument
	ld	a,(EVMODE)
	or	a
	jr	z,DFR1
	call	RELERR		; can't be reloc
	ld	de,0		; null counter to prevent execution
DFR1:	ex	de,hl
	ld	hl,(SYMPTR)
	ld	(PARAMS),hl
	ld	(hl),0FFh	; no parameters
	inc	hl
	ld	(MACBGN),hl
	ld	(hl),e		; store counter value
	inc	hl
	ld	(hl),d
	inc	hl
	ld	(MACPTR),hl	; init macro def pointer

	ld	a,$REPT
	ld	(MACTYP),a
	jp	INCDFL		; increase macro def level

;	SUBTTL	'IRP pseudo-operator'

;	IRP structure as saved in high memory:
;
;	db	STMEXP		; type = MACRO expansion
;	dw	len		; length of the following block:
;	db	$IRP		;  macro type
;	db	clevel		;  saved conditional level
;	dw	index		;  text index
;	db	argidx		;  argument index
;	db	len,'arg1'	;  IRP argument 1
;	db	len,'arg2'	;  IRP argument 2
;	db	...		;
;	db	0FFh		;  end of argument list
;	db	'body line 1',0	;  macro
;	db	'body line 2',0	;   body
;	db	...		;
;	db	EOF		;  end of macro
;
DFIRP:	ld	bc,RECMAX	; hopefully enough - if there are a lot
	call	MCHECK		;  of '%vars' then argument list can
	jp	c,WERROR	;   quickly grow in size!!!
	ld	hl,(SYMPTR)
	ld	(MACPTR),hl
	ld	(PARAMS),hl
	call	ID
	ld	a,(IDLEN)
	or	a		; valid identifier?
	jr	nz,DP1		; yes -> branch
	call	OERROR		; no -> argument error
	ld	de,(MACPTR)
	jr	DP2
DP1:	ld	c,a
	ld	b,0
	ld	hl,IDBUF
	ld	de,(MACPTR)
	ld	(de),a		; store length
	inc	de
	ldir
DP2:	ld	a,0FFh
	ld	(de),a		; 0FFh marks end of params
	inc	de
	ld	(MACBGN),de	; set start address of body
	inc	a
	ld	(de),a		; arg index
	inc	de
	ld	(MACPTR),de
	call	GNC
	cp	','
	jp	nz,OERROR	; comma must follow
	call	GNC
	cp	'<'		; then '<'
	jp	nz,OERROR
DP3:	ld	de,(MACPTR)
	push	de
	inc	de		; length placeholder
	call	GETSTR
	ex	de,hl
	ld	(MACPTR),hl
	pop	de
	push	af
	or	a
	sbc	hl,de
	dec	hl
	ld	a,l
	ld	(de),a		; store length
	pop	af
	jr	c,DP4		; argument error, stop scanning?
	call	GNC
	or	a
	jr	z,DP4		; argument error (missing '>') and exit loop
	cp	'>'
	jr	z,DP5		; success, exit loop
	cp	','
	jr	z,DP3
	call	BACKUP
	jr	DP3
DP4:	call	OERROR
DP5:	ld	hl,(MACPTR)
	ld	(hl),0FFh	; end of argument list
	inc	hl
	ld	(MACPTR),hl
	ld	a,$IRP
	ld	(MACTYP),a
	jp	INCDFL		; increase macro def level
;
;	Skip blanks and get a single argument string until a comma or
;	another blank char is found. Angle brackets and/or quotes can
;	be used to delimit a substring containing spaces or commas.
;
GETSTR:	ld	c,0		; bracket nesting count
	call	GNC
	or	a
	ret	z
	ld	hl,(PTR1)
	dec	hl
GS0:	ld	a,(hl)		; since GNC converts char to uppercase
	inc	hl
	or	a
	jr	z,GS9
	cp	' '
	jr	z,GS6		; delimiters are spaces,
	cp	TAB
	jr	z,GS6		;  tabs
	cp	','
	jr	z,GS6		;   commas
	cp	';'
	jr	z,GS6		;    and semicolons
	cp	'<'
	jr	z,GS1		; opening bracket starts a substring
	cp	'>'
	jr	z,GS2
	cp	'%'		; percent sign followed by a var name or expr
	jr	z,GS5		;  expands to a numeric value
	cp	'!'
	jr	nz,GS7		; exclamation mark means escape char
	ld	a,(hl)
	inc	hl
	or	a
	scf
	jr	z,GS8
GS7:	ld	(de),a		; store char
	inc	de
	cp	27H		;'
	jr	z,GS3
	cp	'"'
	jr	nz,GS0
GS3:	ld	b,a		; get quote char into B
GS4:	ld	a,(hl)
	inc	hl
	or	a
	scf
	jr	z,GS8		; missing closing quote
	ld	(de),a
	inc	de
	cp	b
	jr	nz,GS4		; loop until closing quote found
	jr	GS0

GS1:	inc	c
	dec	c		; first opening bracket?
	jr	nz,GS11	
	inc	c		; yes: increment nesting level
	jr	GS0		;  and remove the bracket
GS11:	inc	c		; no: increment nesting level
	jr	GS7		;  but keep the bracket

GS2:	inc	c		; check bracket nesting
	dec	c		;  error if zero
;;	scf			;; do not tag as error here (caller will)
	jr	z,GS8		; '>' without '<'
	dec	c		; decrement nesting level
	jr	nz,GS7		;  keep the bracket if not zero
	inc	hl		;   else remove the bracket
	or	a		;    and finish
	jr	GS8

GS6:	inc	c
	dec	c		; store delimiters
	jr	nz,GS7		;  if inside substring
GS8:	dec	hl
	ld	(PTR1),hl
	ld	a,0
	ld	(de),a
	ret

GS9:	inc	c		; end of arg string,
	dec	c		;  check for balanced brackets
	jr	z,GS8
	scf			; missing '>'
	jr	GS8

GS5:	; expand %expr

	push	bc
	push	de
	ld	(PTR1),hl
	call	EVALNEW		; evaluate expression
	ld	a,(EVMODE)	; get mode
	and	0C0h+EXTSYM
	jr	z,GS51
	call	RELERR		; value must be Absolute
	and	EXTSYM
	jr	z,GS51
	ld	hl,0		; value is 0 for External
GS51:	pop	de
	call	CVTNUM		; convert to string
	ld	hl,(PTR1)
	pop	bc
	jp	GS0
;
;	Convert value in HL to ASCII string using the current base.
;	Store the result at (DE).
;
CVTNUM:	push	de
	ld	de,(RADIX)
	ld	bc,NUMBUF
	ld	a,0FFh		; end marker
	ld	(bc),a
	inc	bc
CN1:	push	bc
	xor	a
	ld	b,16		; HL=HL/E, remainder in A
DLOOP:	add	hl,hl
	rla
	cp	e
	jr	c,DNEXT
	sub	e
	inc	l
DNEXT:	djnz	DLOOP
	pop	bc
	ld	(bc),a		; save remainder
	inc	bc
	ld	a,h
	or	l		; result 0?
	jr	nz,CN1		; loop if not
	pop	de
CN2:	dec	bc
	ld	a,(bc)		; check digit
	cp	0FFh		; 0FFh is flag for no more
	ret	z		; if so, return
	add	a,90h		; else convert to ASCII
	daa
	adc	a,40h
	daa
	ld	(de),a		; store digit
	inc	de
	jr	CN2		; go back for more

;	SUBTTL	'IRPC pseudo-operator'

;	IRPC structure as saved in high memory:
;
;	db	STMEXP		; type = MACRO expansion
;	dw	len		; length of the following block:
;	db	$IRPC		;  macro type
;	db	clevel		;  saved conditional level
;	dw	index		;  text index
;	db	stridx		;  string index
;	db	len,'string'	;  IRPC argument string
;	db	'body line 1',0	;  macro
;	db	'body line 2',0	;   body
;	db	EOF		;  end of macro
;
DFIRPC:	ld	bc,RECMAX
	call	MCHECK
	jp	c,WERROR
	ld	hl,(SYMPTR)
	ld	(MACPTR),hl	; save macro build pointer
	ld	(PARAMS),hl
	call	ID		; get dummy parameter
	ld	a,(IDLEN)
	or	a		; valid identifier?
	jr	nz,DC1		; yes -> branch
	call	OERROR		; no -> argument error
	ld	de,(MACPTR)
	jr	DC2
DC1:	ld	c,a
	ld	b,0
	ld	hl,IDBUF
	ld	de,(MACPTR)
	ld	(de),a		; store length
	inc	de
	ldir
DC2:	ld	a,0FFh
	ld	(de),a		; 0FFh marks end of params
	inc	de
	ld	(MACBGN),de	; set start address of body
	inc	a
	ld	(de),a		; string index
	inc	de
	ld	(MACPTR),de
	call	GNC
	cp	','
	jp	nz,OERROR	; comma must follow
	ld	de,(MACPTR)
	push	de		; remember address of length field
	inc	de
	call	GNC
	ld	hl,(PTR1)
	dec	hl		; reload the original first char
	ld	a,(hl)		;  as GNC converted it to uppercase
	inc	hl
	cp	'<'
	ld	bc,1		; B = length, C = bracket nesting level
	jr	z,DC5
	dec	c
DC3:	ld	(de),a
	inc	b
	inc	de
	ld	a,(hl)
	or	a
	jr	z,DC4
	cp	' '
	jr	z,DC4
	cp	TAB
	jr	z,DC4
	cp	','
	jr	z,DC4
	cp	';'
	jr	z,DC4
	inc	hl
	jr	DC3
DC4:	ld	(PTR1),hl
	ld	(MACPTR),de
	pop	hl		; pop address of length field
	ld	(hl),b		; save length of string
	ld	a,$IRPC
	ld	(MACTYP),a
	jp	INCDFL		; increase macro def level

DC5:	ld	a,(hl)
	or	a
	jr	z,DC8
	inc	hl
	cp	'>'
	jr	z,DC7
	cp	'<'
	jr	nz,DC6
	inc	c
DC6:	ld	(de),a
	inc	b
	inc	de
	jr	DC5
DC7:	dec	c
	jr	nz,DC6
	jr	DC4
DC8:	call	ERRQ		; missing closing '>'
	jr	DC4

	psect	data

MDFLVL:	defb	0	; nested MACRO definition counter
MACTYP:	defb	0	; type of macro being defined
MACLVL:	defb	0	; macro execution level
MACBGN:	defw	0	; begin of macro body
MACPTR:	defw	0	; macro build ptr
PARAMS:	defw	0	; pointer to dummy parameters during MACRO build
ARGS:	defw	0	; pointer to argument list during MACRO expansion
LCLFLG:	defb	0	; LOCAL variable flag
LCLNUM:	defw	0	; LOCAL label number for next MACRO expansion
LBASE:	defw	0	; LOCAL label base number for executing MACRO
LCOUNT:	defb	0	; number of LOCAL variables in a MACRO
RCOUNT:	defb	0	; REC char count during MACRO expansion
NUMBUF:	defs	17	; buffer for numeric conv. (max size = binary base)

;	end
