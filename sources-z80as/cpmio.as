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

*include	ZSM.INC

;public
	global	UCASE,GNB,WNB,WNB2,CLINE,WLINE,REWIND,INCMEM
	global	CLSINP,CLOSE1,CLOSE2,OPNLIB,CLSLIB,INCLVL
;extern
	global	Z80ASM,HOFNAM,LFLAG,OFLAG,QFLAG,JFLAG
	global	ERRFLG,CMPHD,MAXMEM,SYMTBL,VALID,MALLOC,MFREE
	global	FNDREC,CNV2HX,DEFCPU,RADIX,UMODE
	global	CMNPTR

	global	SYMPTR,DSPTR

	global	PTR1,ID,IDLEN,INT,INTBUF,VAL,SYMMOD,ADDSYM,IDADR

	global	START

BDOS	equ	5		; BDOS entry point
BOOT	equ	0		; warm boot
CPMFCB	equ	5Ch		; default CP/M FCB
CPMBUF	equ	80h

IBUFSZ	equ	512		; input buffer size
OBUFSZ	equ	256		; object buffer size
LBUFSZ	equ	512		; listing buffer size

	psect	text

	SUBTTL	Initializations and command loop
;
;	System-dependent initializations
;
START:	ld	sp,STACK	; setup local stack
	ld	hl,(BDOS+1)	; get top of memory
	ld	l,0		; trim to page boundary
	dec	hl
	ld	(MAXMEM),hl	; save max memory address
	ld	hl,$MEMRY
	ld	(SYMTBL),hl	; save start of symbol table
	ld	hl,CPMBUF
	ld	a,(hl)		; command tail present?
	or	a
	jr	nz,S2		; branch if yes
	ld	hl,ARGSMSG
	call	CLINE
	jp	BOOT
S2:	call	PROCESS		; parse and process the command
	jp	BOOT		; else exit program
;
; Get command line options, if present
;	returns CARRY=1 : wrong option or empty line
;		CARRY=0 : HL=pointer of sourcefile
;
GETOPT:	
next:
	call	SKIPB		; skip blanks
	scf
	ret	z		;EOL, return (CARRY=1)
	cp	'-'		; option switch?
	jr	z,option
	or	a		; found non-zero char, must be the filename...
	ret
option:
	inc	hl
	ld	a,(hl)
	call	UCASE
	cp	'L'
	jr	nz,1f
	ld	a,'='
	ld	(LFLAG),a	; set listing flag, using name of input file
	ld	de,LSTFILE
	call	SETFILE
	jr	next
1:
	cp	'O'
	jr	nz,2f
	ld	de,OBJFILE
	call	SETFILE
	jr	next
2:
	cp	'N'
	jr	nz,3f		;ignore N option
	inc	hl
	jr	next
3:
	cp	'J'
	jr	nz,4f
	ld	a,0FFH
	ld	(JFLAG),a	;set JFLAG 
	inc	hl
	jr	next
4:
	cp	'Q'
	jr	nz,5f
	ld	a,0FFH
	ld	(QFLAG),a	;set "brief" mode
	inc	hl
	jr	next
5:
	scf			;wrong option
	ret
;
;	Save a possible filename pointer
;	HL=pointer of option flag
;	DE=storage
;
SETFILE:
	inc	hl		;skip option char
	ld	a,(hl)		;get next char
	or	a		;return if blank, TAB or EOL
	ret	z
	cp	' '
	ret	z
	cp	TAB
	ret	z
	ld	a,l		;else store the pointer
	ld	(de),a
	inc	de
	ld	a,h
	ld	(de),a
loop:	inc	hl		;and go to next blank, TAB or EOL
	ld	a,(hl)
	or	a
	ret	z
	cp	' '
	ret	z
	cp	TAB
	ret	z
	jr	loop
;
;	Parse the command line, open/create files and call the assembler
;
PROCESS:
	ld	hl,(SYMTBL)
	ld	(SYMPTR),hl
	ld	(hl),0		; reset symbol table
	ld	hl,(MAXMEM)
	ld	(DSPTR),hl
	ld	(hl),0		; reset dynamic memory storage
				; setup default options
	ld	a,'Z'		
	ld	(LFLAG),a	; no listing file 
	ld	a,'='
	ld	(OFLAG),a	; output obj file name equal to source file name
	ld	a,1
	ld	(UMODE),a	; Undefined symbols default to External
	xor	a
	ld	(DEFCPU),a	; CPU is Z80
	ld	hl,10
	ld	(RADIX),hl	; default radix = 10 (for /D option)

	ld	hl,CPMBUF
	ld	e,(hl)
	ld	d,0
	inc	hl
	push	hl
	add	hl,de
	ld	(hl),0		; terminate command line with a null
	pop	hl

	call	GETOPT		;get options
	jr	nc,getfile

ERR:	call	CMDERR		;something get wrong, return
	ret

getfile:			;HL=source filename pointer
	ld	de,FCB1		; set up source FCB
	call	MKFCB
	jr	c,ERR
	call	TSTFCB
	jr	c,ERR
	cp	'Y'
	ld	hl,SM5
	jp	z,CLINE		; input device can't be LPT:
	ld	(IFLAG),a
	ld	hl,ASMEXT
	call	ADDEXT		; set up extension
;
;	OBJ file
;
	ld	hl,(OBJFILE)	;-Oobjfile was used?
	ld	a,h
	or	l
	jr	z,copysrc
				;yes
	ld	de,FCB2		;OBJ file FCB
	call	MKFCB
	jp	c,CMDERR
	call	TSTFCB
	jp	c,CMDERR
	ld	(OFLAG),a
	ld	hl,RELEXT
	call	ADDEXT		; set up extension
	jr	seelist

copysrc:
	ld	hl,FCB1
	ld	de,FCB2		; setup implicit OBJ output file
	ld	bc,36
	ldir
	ld	hl,RELEXT
	ld	de,FCB2+9
	ld	bc,3
	ldir
	ld	a,'@'
	ld	(OFLAG),a

seelist:
;
;	LST file
;
	ld	hl,(LSTFILE)	;-Llistfile was used?
	ld	a,h
	or	l
	jr	z,seelflag
				;yes
	ld	de,FCB3		;LST file FCB
	call	MKFCB
	jp	c,CMDERR
	call	TSTFCB
	jp	c,CMDERR
	ld	(LFLAG),a
	ld	hl,PRNEXT
	call	ADDEXT		; set up extension
	jr	donefiles

seelflag:
	ld	a,(LFLAG)
	cp	'='
	jr	nz,donefiles

	ld	hl,FCB1
	ld	de,FCB3		; setup implicit LST output file
	ld	bc,36
	ldir
	ld	hl,PRNEXT
	ld	de,FCB3+9
	ld	bc,3
	ldir
	ld	a,'@'
	ld	(LFLAG),a
;
donefiles:
	ld	a,(QFLAG)
	or	a
	jr	nz,nosignon
	ld	hl,VSNMSG	; sign on message
	call	CLINE		; display on console
nosignon:

;	Header line message

PR5A:	ld	hl,FCB1+1
	ld	de,HOFNAM
	ld	bc,8
	ldir			; set file name

;	Open/create files

PR7:	ld	a,(IFLAG)
	cp	'X'
	jr	z,PR71
	ld	de,FCB1
	call	OPNFIL		; open source file
	ld	hl,SM1		; source file not found
	jp	z,CLINE		; on error, msg and return

PR71:	ld	de,FCB3
	ld	a,(LFLAG)
	cp	'@'
	jr	nz,PR8
	call	CREFIL		; create listing file
	ld	hl,SM3		; can't create listing file
	jp	z,CLINE		; on error, msg and return

PR8:	ld	de,FCB2
	ld	a,(OFLAG)
	cp	'@'
	jr	nz,PR9
	call	CREFIL		; create object file
	ld	hl,SM2		; can't create object file
	jp	z,CLINE		; on error, msg and return

PR9:	ld	hl,0
	ld	(IBCNT),hl	; init char counters
	ld	(IBLEN),hl
	ld	(IOB2+BUFCNT),hl
	ld	(IOB3+BUFCNT),hl
	xor	a
	ld	(INCLVL),a
	ld	(EFLAG),a
	call	Z80ASM		; assemble file

	ld	a,(QFLAG)
	or	a
	ret	nz
	ld	hl,ENDMSG	; sign off
	jr	CLINE		; msg and return

CMDERR:	ld	hl,SM4		; command syntax error
	jr	CLINE		; output message and return

; Get next non-blank char from command line

SKP:	inc	hl
SKIPB:	ld	a,(hl)
	or	a
	ret	z
	cp	' '
	jr	z,SKP
	cp	TAB
	jr	z,SKP
	ret

; Test FCB and return code in reg A:
;  'Z' if FCB is empty
;  '@' if FCB specifies a valid disk file
;  'X' if console device
;  'Y' if printer device
; CY set on error (invalid device name)

TSTFCB:	inc	de
	ld	a,(de)
	dec	de
	cp	' '
	ld	a,'Z'
	ret	z		; empty FCB
	ld	a,(hl)
	cp	':'
	jr	z,T1
	ld	a,'@'		; disk file
	or	a
	ret
T1:	inc	hl
	push	hl
	call	TSTDEV
	pop	hl
	ret

TSTDEV:	ld	hl,DEVNAM
TEST1:	ld	a,(hl)
	or	a		; end of table?
	scf
	ret	z
	push	de
	inc	de
	ld	b,3
TEST2:	ld	a,(de)
	cp	(hl)
	jr	nz,TEST3
	inc	hl
	inc	de
	djnz	TEST2
	pop	de
	ld	a,(hl)
	add	a,40h
	ret			; note CY clear
TEST3:	inc	hl
	djnz	TEST3
	inc	hl
	pop	de
	jr	TEST1

	SUBTTL	Console, Printer and File I/O

;	ENTRY - Used to call BDOS - Saves and restores registers

ENTRY:	push	bc
	push	de
	push	hl
	push	ix
	call	BDOS
	pop	ix
	pop	hl
	pop	de
	pop	bc
	ret

;	WLINE - Write line to list device and append newline
;	HL -> buffer, term = null

WLINE:	ld	a,(hl)
	or	a
	jr	z,WCRLF
	ld	e,a
	ld	c,5
	call	ENTRY
	inc	hl
	jr	WLINE
WCRLF:	ld	e,CR
	ld	c,5
	call	ENTRY
	ld	e,LF
	ld	c,5
	jr	ENTRY

;	CLINE - Write line to console and append newline
;	HL -> buffer, term = null

CLINE:	ld	a,(hl)
	or	a
	jr	z,CRLF
	ld	e,a
	ld	c,2
	call	ENTRY
	inc	hl
	jr	CLINE
CRLF:	ld	e,CR
	ld	c,2
	call	ENTRY
	ld	e,LF
	ld	c,2
	jr	ENTRY

;	OPNFIL - Open file
;	DE -> FCB
;	Z=0 success, Z=1 failure

OPNFIL:	ld	c,15
	call	ENTRY
	inc	a		; test for 255
	ret

;	CLSFIL - Close file
;	DE -> FCB
;	Z=0 success, Z=1 error

CLSFIL:	ld	c,16
	call	ENTRY
	inc	a
	ret

;	DREAD - Read disk sector
;	DE -> FCB
;	Z=0 error or EOF, Z=1 normal

DREAD:	ld	c,33		; note: read random record
	call	ENTRY
	or	a
	ret

;	DWRITE - Write disk sector
;	DE -> FCB
;	Z=0 error, Z=1 normal

DWRITE:	ld	c,21
	call	ENTRY
	or	a
	ret

;	CREFIL - Create file
;	Reel # assummed to be set
;	DE -> FCB
;	Z=0 normal, Z=1 error

CREFIL:	push	de
	ld	c,19		; delete file
	call	ENTRY
	pop	de
	ld	c,22
	call	ENTRY
	inc	a
	ret

;	DMASET - Set DMA address
;	Buffer address in DE

DMASET:	ld	c,26
	jr	ENTRY

;	Convert character in A to uppercase

UCASE:	cp	'a'
	ret	c
	cp	'z'+1
	ret	nc
	and	5Fh
	ret

;	GNB - Get next byte (FCB1)

GNB:	push	hl
	push	de
	ld	hl,(IBLEN)
	ld	de,(IBCNT)
	call	CMPHD		; see if end of buffer
	call	z,REFILL	; refill buffer if necessary
	ld	hl,INBUF
	add	hl,de
	ld	a,(hl)		; get byte
	and	7Fh		; strip parity bit
	inc	de
	ld	(IBCNT),de
	pop	de	
	pop	hl
	ret

;	REFILL - (Re)fill input buffer

REFILL:	push	bc
	ld	a,(IFLAG)
	cp	'X'
	jr	z,RE4
	ld	hl,(FCB1+33)
	ld	(IRECNO),hl
	ld	de,INBUF	; set up pointer for DMASET
	ld	hl,IBUFSZ	; input buffer size
RE1:	push	hl
	push	de
	call	DMASET		; set up DMA address
	ld	de,FCB1
	call	DREAD		; go read a sector
	ld	hl,(FCB1+33)
	inc	hl
	ld	(FCB1+33),hl	; point to next record
	pop	de
	pop	hl
	jr	z,RE2
	ld	a,EOF
	ld	(de),a
	jr	RE3
RE2:	ex	de,hl
	ld	bc,128
	add	hl,bc
	ex	de,hl		; update pointer
	or	a
	sbc	hl,bc
	ld	a,h
	or	l
	jr	nz,RE1		; branch back if more to do
RE3:	ex	de,hl
	ld	hl,IBUFSZ
	or	a
	sbc	hl,de
	ld	(IBLEN),hl
RE6:	ld	de,0
	ld	(IBCNT),de	; set up next data pointer
	pop	bc		; note: returns DE = 0
	ret

RE4:	call	GETLN
	ld	hl,CPMBUF
	ld	de,INBUF
	ld	c,(hl)
	ld	b,0
	push	bc
	inc	hl
	ld	a,b
	or	c
	jr	z,RE5
	ldir
RE5:	ex	de,hl
	ld	(hl),CR
	inc	hl
	ld	(hl),LF
	inc	hl
	ld	(hl),0
	pop	bc
	inc	bc
	inc	bc
	ld	(IBLEN),bc
	jr	RE6

GETLN:	ld	hl,CPMBUF-1
	ld	(hl),126
	ex	de,hl
	ld	c,10
	call	ENTRY		; get command line
	jp	CRLF


;	REWIND - Rewind input file

REWIND:	ld	a,(INCLVL)
	or	a
	jr	z,REW1
	call	CLSLIB		; close any open INCLUDE files
	jr	REWIND
REW1:	ld	hl,0
	ld	(FCB1+33),hl	; reset r0,r1
	ld	hl,0
	ld	(IBCNT),hl	; reset char pointer
	ld	(IBLEN),hl
	ret

;	CLSINP - Close input file

CLSINP:	ld	a,(IFLAG)
	sub	'X'
	ret	z
	ld	de,FCB1
	jp	CLSFIL		; !!!TODO: close include files?
				; (are there any still open?)

;	WNB - Write next byte (PRN)
;	Byte in A reg

WNB:	ld	iy,EFLAG
	bit	2,(iy)
	scf
	ret	nz
	push	ix
	and	7Fh		; strip parity bit
	ld	ix,IOB3
	call	FWRITE
	pop	ix
	ret	nc
	set	2,(iy)
	ret

;	WNB2 - Write next byte (REL)

WNB2:	ld	iy,EFLAG
	bit	1,(iy)
	scf
	ret	nz
	push	ix
	ld	ix,IOB2
	call	FWRITE
	pop	ix
	ret	nc
	set	1,(iy)
	ret

;	FWRITE - Write byte to file
;	Byte in reg A
;	I/O block address in reg IX

FWRITE:	push	hl
	push	de
	push	bc
	ld	e,(ix+BUFCNT)
	ld	d,(ix+BUFCNT+1)
	ld	l,(ix+BUFADR)
	ld	h,(ix+BUFADR+1)
	push	hl
	add	hl,de
	ld	(hl),a		; store byte
	inc	de
	ld	l,(ix+BUFSZ)
	ld	h,(ix+BUFSZ+1)
	call	CMPHD		; at end of buffer?
	pop	bc
	jr	nz,WNB1		; branch if not
	ld	e,c		; buffer address in DE, size in HL
	ld	d,b
WNB0:	push	de
	call	DMASET
	ld	e,(ix+FCBADR)
	ld	d,(ix+FCBADR+1)
	call	DWRITE
	pop	de
	jr	nz,WRERR	; on error, print message and return
	ld	bc,128
	ex	de,hl
	add	hl,bc
	ex	de,hl
	or	a
	sbc	hl,bc
	ld	a,h
	or	l
	jr	nz,WNB0
	ld	de,0
WNB1:	ld	(ix+BUFCNT),e
	ld	(ix+BUFCNT+1),d
	pop	bc
	pop	de
	pop	hl
	or	a
	ret

WRERR:	ld	hl,SM6		; file write error
	call	CLINE		; msg and return
	pop	bc
	pop	de
	pop	hl
	scf
	ret

;	CLOSE1 - Close PRN file

CLOSE1:	push	ix
	ld	ix,IOB3
	call	CLOSE
	pop	ix
	ret

;	CLOSE2 - Close REL file

CLOSE2:	push	ix
	ld	ix,IOB2
	call	CLOSE
	pop	ix
	ret

CLOSE:	ld	a,(ix+BUFCNT)
	or	(ix+BUFCNT+1)
	jr	z,CLS1
	ld	a,EOF
	call	FWRITE
	jr	CLOSE
CLS1:	ld	e,(ix+FCBADR)
	ld	d,(ix+FCBADR+1)
	jp	CLSFIL

;	OPNLIB - Open MACLIB or INCLUDE file
;	HL points to file name
;
;	Input file structure as saved in high memory:
;
;	defb	STINPF		; type = include/input file state
;	defw	len		; total length of the following fields:
;	defb	iflag		;  input device code
;	defw	recno		;  current record number
;	defw	ci		;  current char index
;	defs	36		;  current FCB

OPNLIB:	ld	a,(INCLVL)
	cp	5		; check nested include level
	jr	nc,FNERR1	; error if above maximum

	push	hl
	ld	bc,1+2+2+36	; iflag + recno + char index + FCB
	ld	e,STINPF	; type = file
	call	MALLOC		; allocate block
	pop	de
	ret	c		; on error, return

	push	hl		; save block address
	push	de		; save pointer to file name
	ld	a,(IFLAG)
	ld	(hl),a		; store input device code
	inc	hl
	ld	bc,(IRECNO)
	ld	(hl),c		; store starting record number
	inc	hl
	ld	(hl),b
	inc	hl
	ld	bc,(IBCNT)
	ld	(hl),c		; store char index
	inc	hl
	ld	(hl),b
	inc	hl

	ex	de,hl		; DE = addr to store FCB
	ld	hl,FCB1
	ld	bc,36
	ldir			; store FCB
	pop	hl		; restore ptr to filename
	ld	de,FCB1		; FCB address in DE
	call	MKFCB		; make FCB
	jr	c,FNERR		; bad file name

	push	hl
	ld	hl,ASMEXT	; default include extension is .MAC
	call	ADDEXT
	pop	hl
	call	OPNFIL
	jr	z,FNERR		; file not found

	ex	(sp),hl		; drop block address
	ld	hl,0
	ld	(IBCNT),hl	; init pointers
	ld	(IBLEN),hl
	ld	a,'@'
	ld	(IFLAG),a
	ld	hl,INCLVL
	inc	(hl)
	pop	hl		; restore record pointer
	or	a
	ret

FNERR:	ex	(sp),hl		; restore block address, save record pointer
	push	hl
	ld	de,1+2+2
	add	hl,de
	ld	de,FCB1
	ld	bc,36
	ldir			; restore old FCB
	pop	hl
	dec	hl
	dec	hl
	dec	hl
	call	MFREE		; free allocated block
	pop	hl		; restore record pointer
FNERR1:	ld	a,'V'
	ld	(ERRFLG),a	; set error flag
	scf
	ret

;	MKFCB - Create FCB from string
;	HL points to string
;	DE points to FCB

MKFCB:	call	CLRFCB
	ld	a,(hl)
	or	a
	scf
	ret	z
	push	de
	call	UCASE
	sub	'A'-1
	ld	b,a		; may be disk name
	inc	hl
	ld	a,(hl)
	cp	':'
	jr	z,MF3
	dec	hl
	jr	MF4
MF3:	ld	a,b
	ld	(de),a		; store disk name
	inc	hl
MF4:	inc	de
	ld	b,8
	call	GETNAM
	ld	a,(hl)
	cp	'.'
	jr	nz,MF5
	inc	hl
MF5:	ld	b,3
	call	GETNAM
	pop	de
	xor	a
	ret
	
CLRFCB:	push	de
	ld	b,36
	xor	a
CLRF1:	ld	(de),a
	inc	de
	djnz	CLRF1
	pop	de
	ret

GETNAM:	ld	a,(hl)
	call	UCASE
	call	VALID
	jr	c,SFILL
	cp	'.'
	jr	z,SFILL
	ld	(de),a
	inc	hl
	inc	de
	djnz	GETNAM
SKIP:	ld	a,(hl)
	call	UCASE
	call	VALID
	ret	c
	cp	'.'
	ret	z
	jr	SKIP
SFILL:	ld	a,' '
SF:	ld	(de),a
	inc	de
	djnz	SF
	ret

ADDEXT:	push	de
	ex	de,hl
	ld	bc,8+1
	add	hl,bc
	ld	a,(hl)
	cp	' '		; extension present?
	jr	nz,ADDE1	; return if not
	ex	de,hl
	ld	bc,3
	ldir			; else set it
ADDE1:	pop	de
	ret

;	CLSLIB - Close current MACLIB or INCLUDE file

CLSLIB:	ld	a,(INCLVL)
	or	a
	ret	z		; nothing to do, no INCLUDEs active
	dec	a
	ld	(INCLVL),a

	ld	de,FCB1
	call	CLSFIL		; close file

	ld	c,STINPF
	call	FNDREC		; get previous file record
;	jp	c,...		; should not happen

	push	hl		; save start of file record
	inc	hl
	inc	hl
	inc	hl

	ld	a,(hl)		; get input device code
	ld	(IFLAG),a
	inc	hl

	ld	e,(hl)		; get starting record number
	inc	hl
	ld	d,(hl)
	inc	hl
	push	de

	ld	e,(hl)		; get char index
	inc	hl
	ld	d,(hl)
	inc	hl
	ld	(IBLEN),de	; in case we were reading from terminal
	push	de

	ld	de,FCB1
	ld	bc,36
	ldir			; restore FCB

	pop	hl		; get char index into HL
	pop	de		; and starting record number in DE
	cp	'X'		; IFLAG still in A
	jr	z,ICLS2

	ld	bc,-128
ICLS1:	add	hl,bc		; reduce starting record number
	inc	de
	jr	c,ICLS1
	dec	de
	ld	(FCB1+33),de
	ld	bc,128
	add	hl,bc
	push	hl
	call	REFILL		; refill input buffer
	pop	hl
ICLS2:	ld	(IBCNT),hl	; set input char pointer

	pop	hl		; pop start of record
	call	MFREE		; release storage

	or	0FFh		; NZ
	ret

;	INCMEM - No memory increase function in CP/M (already using
;	         all available memory)

INCMEM:	scf
	ret

	psect	data

ENDMSG:	defm	'Finished.'
	defb	0

SM1:	defm	'Source file not found'
	defb	0
SM2:	defm	'Unable to create object file'
	defb	0
SM3:	defm	'Unable to create listing file'
	defb	0
SM4:	defm	'Command syntax error'
	defb	0
SM5:	defm	'Illegal input device'
	defb	0
SM6:	defm	'Output file write error'
	defb	0

ASMEXT:	defm	'AS '
RELEXT:	defm	'OBJ'
PRNEXT:	defm	'LST'

DEVNAM:	defm	'CON'
	defb	18h
	defm	'TTY'
	defb	18h
	defm	'LST'
	defb	10h
	defm	'LPT'
	defb	10h
	defb	0

OBJFILE:defw	0
LSTFILE:defw	0

VSNMSG:	defm	'Z80AS Macro-Assembler V'
	defb	VER1
	defm	'.'
	defb	VER2
	defb	0

ARGSMSG:defm	'Usage: Z80AS [-L[listfile]] [-Oobjfile] [-J] sourcefile'
	defb	0

; I/O block offset definitions

BUFCNT	equ	0		; buffer index (2 bytes)
BUFSZ	equ	BUFCNT+2	; buffer size (2 bytes)
FCBADR	equ	BUFSZ+2		; file control block address (2 bytes)
BUFADR	equ	FCBADR+2	; buffer address (2 bytes)
IOBSZ	equ	BUFADR+2	; I/O block size

IOB2:	defs	2		; buffer char counter
	defw	OBUFSZ		; buffer size
	defw	FCB2		; FCB address (REL)
	defw	BUF2		; buffer address

IOB3:	defs	2		; buffer char counter
	defw	OBUFSZ		; buffer size
	defw	FCB3		; FCB address (PRN)
	defw	BUF3		; buffer address

IFLAG:	defs	1		; input device code
FCB1:	defs	36		; file control block (source)
INBUF:	defs	IBUFSZ		; .MAC input buffer
IBCNT:	defs	2		; index of next char in input buffer
IBLEN:	defs	2		; number of valid bytes in input buffer
IRECNO:	defs	2		; starting record number of input buffer

FCB2:	defs	36		; file control block (REL)
BUF2:	defs	OBUFSZ		; .REL output buffer

FCB3:	defs	36		; file control block (PRN)
BUF3:	defs	OBUFSZ		; .PRN output buffer

INCLVL:	defs	1		; nested INCLUDE level

EFLAG:	defs	1		; I/O error output flag

	defs	128
STACK	equ	$		; Z80 stack

	global	$MEMRY

$MEMRY:	

;	end	START
