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
	SUBTTL	Dynamic memory management routines

*include	ZSM.INC

;public
	global	MCHECK,MACCHK,MALLOC,MFREE,CMPHD,FNDREC,FNDNXT,GBCOL
;extern
	global	INCMEM,MAXMEM,DSPTR,SYMPTR,MACPTR,ERRFLG

	psect	text

;	MCHECK - Check for available memory
;	BC = size in bytes
;	Returns CY flag set if not enough memory

MCHECK:	push	hl
	ld	hl,(SYMPTR)
MCHK1:	add	hl,bc
	push	de
	ex	de,hl
	ld	hl,(DSPTR)
	call	CMPHD
	ccf
	call	c,INCMEM
	pop	de
	pop	hl
	ret

;	MACCHK - Check for available memory, this one is used when
;	building a MACRO.
;	BC = size in bytes
;	Returns CY flag set if not enough memory

MACCHK:	push	hl
	ld	hl,(MACPTR)
	jr	MCHK1		; continue via common code

;	MALLOC - Allocate a block of memory
;	BC = size in bytes, not including overhead
;	E = record type
;	Returns HL pointing to data area of allocated block
;	CY flag set if not enough memory

MALLOC:	inc	bc
	inc	bc
	inc	bc		; overhead = 3 bytes (type + size)
	call	MCHECK
	jr	nc,M1		; branch if OK
	ld	a,'W'		; else error
	ld	(ERRFLG),a
	ret
	
M1:	ld	hl,(DSPTR)	; get updated top of available memory
	or	a
	sbc	hl,bc		; alloc block
	ld	(DSPTR),hl	; set new top of available memory
	ld	(hl),e		; store record type
	inc	hl
	dec	bc
	dec	bc
	dec	bc
	ld	(hl),c		; store size, not including overhead
	inc	hl
	ld	(hl),b
	inc	hl
	ret

;	MFREE - Release allocated memory
;	HL = start of block to free

MFREE:	push	hl
	ld	de,(DSPTR)
	or	a
	sbc	hl,de		; get length of storage under current block
	ld	c,l		; BC = len
	ld	b,h
	pop	hl
	push	hl
	inc	hl
	ld	e,(hl)		; get record length
	inc	hl
	ld	d,(hl)
	add	hl,de		; point to last byte
	ex	de,hl		; DE = dst
	pop	hl
	dec	hl		; HL = src
	ld	a,b
	or	c
	jr	z,M2		; branch if nothing to move
	lddr			; move lower blocks to free space
M2:	ex	de,hl
	inc	hl
	ld	(DSPTR),hl	; set new top of memory
	ret

;	CMPHD - Compare HL:DE
;	Returns Z flag set if HL == DE, CY flag set if HL > DE

CMPHD:	ld	a,d
	cp	h
	ret	nz
	ld	a,e
	cp	l
	ret

;	FNDREC - Find record of specified type in high memory storage
;	C has record type

FNDREC:	ld	hl,(DSPTR)
FNDR1:	ld	a,(hl)		; get type
	or	a		; null?
	scf
	ret	z		; return error - not found
	cp	c		; found?
	ret	z		; return success
FNDNXT:	inc	hl
	ld	e,(hl)		; else get length
	inc	hl
	ld	d,(hl)
	inc	hl
	add	hl,de		; point to next record
	jr	FNDR1

;	GBCOL - Perform garbage collection, remove all deleted entries.

GBCOL:	ld	c,STDEL
	call	FNDREC
	ret	c
	call	MFREE
	jr	GBCOL

;	end
