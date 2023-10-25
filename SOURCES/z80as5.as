*include	zsm.inc

	global	Z80ASM,HOFNAM,LFLAG,OFLAG,QFLAG,DBWFLG,VAL,XFLAG,DFLAG
	global	IDBUF,ERRFLG,BACKUP,EVFLGS,GNC,CPU,ADDEXT
	global	PC,PCFLAG,PTR1,UFLAG,SYMTBL,CURSEG,CONDSP
	global	REC,LEN,IDLEN,OERROR,SYMADR,EVMODE
	global	MAXMEM,SYMPTR,SYMMOD,IDADR,NEWSYM,GETPPC
	global	RADIX,ERRQ,VALERR,REQCHR,CHK8U,GNR
	global	DSPTR,OPCODES,OPLEN,MACFLG,CURLNE
	global	DBWFLG,DSFLAG,EQUFLG,ERRCNT,HDRBUF,HOFEND
	global	HOFMSG,HOFPG,IFLIST,NOLIST,LSTOPT,LENDS
	global	TITLEB,SBTTLB,RESETP
	global	LBLFLG,CNDSTK,CLEVEL,UMODE,DEFCPU
	global	ID_OR_NUMBER
	global	TempCnt
	global	JFLAG,JPASS,JCOUNT
	global	C1N,C2N,C3N,atof,ENDADR,EXTCHN,CMNPTR,ENDMARK,ENDMOD,fperr
	global  FLAG_T,FLAG_D,FLAG_B,FLAG_C1,FLAG_C2,FLAG_C3
	global	ASEGPC,CSEGPC,DSEGPC,BSEGPC,CUST1SEGPC,CUST2SEGPC,CUST3SEGPC

	global	ID
	global	IDTOUP
	global	SYMLUK
	global	SWITCH
	global	MAXLNE
	global	WLOC
	global	ADDSYM
	global	MERROR
	global	SETMDF
	global	OPNLIB
	global	DEFMAC
	global	DFREPT
	global	DFIRP
	global	DFIRPC
	global	MACLVL
	global	ENDMAC
	global	EVALNEW
	global	MCHECK
	global	WERROR
	global	CLINE
	global	UCASE
	global	RELERR
	global	LSTB
	global	WOBJ
	global	WOBJ16
	global	LSTW
	global	SearchJRtab
	global	CL1
	global	CL8
	global	CL9
	global	S321
	global	S500
	global	S520
	global	S360
	global	S370
	global	S380
	global	S390
	global	CL10
	global	S662
	global	CL11
	global	CL12
	global	CL13
	global	CL14
	global	CL15
	global	CL16
	global	CL17
	global	CL18
	global	CL19
	global	CL20
	global	CL21
	global	CL22
	global	CL23
	global	CL24
	global	CL25
	global	CL26
	global	CL27
	global	S391
	global	S392
	global	S530
	global	S540
	global	S550
	global	S560
	global	S570
	global	S580
	global	S590
	global	S600
	global	S610
	global	S620
	global	S630
	global	S640
	global	S650
	global	S660
	global	S670
	global	EFLG
	global	ERRQ
	global	EMITB
	global	REQ8U
	global	EMITV
	global	AM9511F
	global	LOCFLG
	global	SAVEPC
	global	PASSNO
	global	COMNTC
	global	CMPSYM
	global	OPCODE
	global	TYPTBL
	global	SWITCH
	global	SAVVAL
	global	EMITSV
	global	EMITVR
	global	MERROR
	global	SETMDF
	global	JOPTDIS
	global	SAVVARS
	global	AddToJRtab
	global	CheckPSECTS

	psect	text
;
;       LIST control operator
;
;       ON       Enable listing
;       OFF      Disable listing
;       COND     Display FALSE conditional code
;       NOCOND   Don't display FALSE conditional code
;       SYMBOL   Produce symbols in output
;       NOSYMBOL Don't produce symbols in output
;       SORT     Sort symbols in output
;       NOSORT   Don't sort symbols in output
;       MACROS   Display full MACRO expansions
;       XMACROS  Display only MACRO lines that generate code
;       NOMACROS Don't display MACRO expansions
;
S360:   ld      ix,LSTOPT       ; IX points to LIST options byte
        ld      a,(OPCODE+1)    ; test for shortcuts
        dec     a
        jp      p,S362
S361:   call    ID              ; else get operand
	call	IDTOUP
        ld      a,(IDLEN)
        or      a               ; valid?
        jp      z,OERROR        ; early error exit
        ld      de,LSTOPS       ; LIST operands
        ld      c,1
        call    SYMLUK          ; lookup name
        jp      c,OERROR        ; error if not found
        ld      a,(hl)          ; get value
        ld      hl,S363
        push    hl              ; push return address
S362:   ld      hl,LSTTAB
        jp      SWITCH
S363:   call    GNC
        cp      ','             ; more options?
        jr      z,S361          ; loop if yes
        ret

	psect	data

;
LSTOPS: defb    2
        defm    'ON'
        defb    0
        defb    3
        defm    'OFF'
        defb    1
        defb    4
        defm    'COND'
        defb    2
        defb    6
        defm    'NOCOND'
        defb    3
        defb    6
        defm    'SYMBOL'
        defb    4
        defb    8
        defm    'NOSYMBOL'
        defb    5
        defb    4
        defm    'SORT'
        defb    6
        defb    6
        defm    'NOSORT'
        defb    7
        defb    6
        defm    'MACROS'
        defb    8
        defb    7
        defm    'XMACROS'
        defb    9
        defb    8
        defm    'NOMACROS'
        defb    10
        defb    0
;
LSTTAB: defw    LST10           ; ON            .LIST
        defw    LST20           ; OFF           .XLIST
        defw    LST30           ; COND          .LFCOND
        defw    LST40           ; NOCOND        .SFCOND
        defw    LST50           ; SYMBOL
        defw    LST60           ; NOSYMBOL
        defw    LST70           ; SORT
        defw    LST80           ; NOSORT
        defw    LST90           ; MACROS        .LALL
        defw    LST91           ; XMACROS       .XALL
        defw    LST92           ; NOMACROS      .SALL

	psect	text

;
;       ON
;
LST10:  set     LISTFL,(ix)
        ret
;
;       OFF
;
LST20:  res     LISTFL,(ix)
        ret
;
;       COND
;
LST30:  set     CONDFL,(ix)
        ret
;
;       NOCOND
;
LST40:  res     CONDFL,(ix)
        ret
;
;       SYMBOL
;
LST50:  set     SYMBLS,(ix)
        ret
;
;       NOSYMBOL
;
LST60:  res     SYMBLS,(ix)
        ret
;
;       SORT
;
LST70:  set     SORTFL,(ix)
        ret
;
;       NOSORT
;
LST80:  res     SORTFL,(ix)
        ret
;
;       MACROS
;
LST90:  set     MACRFL,(ix)
        res     XMACFL,(ix)
        ret
;
;       XMACROS
;
LST91:  set     MACRFL,(ix)
        set     XMACFL,(ix)
        ret
;
;       NOMACROS
;
LST92:  res     MACRFL,(ix)
        ret
;
;       TITLE, SUBTTL
;
S370:   ld      a,(OPCODE+1)
        or      a
        ld      hl,TITLEB       ; point to title buffer
        ld      b,80            ; maximum count
        jr      z,S371          ; branch if TITLE
        ld      hl,SBTTLB       ; else is SUBTTL
        ld      b,60            ; maximum count
S371:   call    GNC             ; get next non blank
        ld      de,(PTR1)       ; get REC pointer into DE
        or      a               ; null title?
        jr      z,S374          ; finish if so
        ld      c,a
        cp      '"'
        jr      z,S372          ; quotes are optional,
        cp      27H             ;' and removed if present
        jr      z,S372
        ld      c,0
        dec     de
S372:   ld      a,(de)          ; get a char
        or      a               ; end of line
        jr      z,S374          ; exit if so
        inc     de              ; update REC pointer
        cp      c
        jr      z,S374
        ld      (hl),a          ; store character
        inc     hl              ; update buffer pointer
        djnz    S372            ; count down chars
S374:   cp      c
        call    nz,ERRQ
        ld      (hl),0          ; end with a null
        ld      (PTR1),de       ; update REC pointer
        ld      a,(OPCODE+1)
        or      a
        ret     nz              ; return if SUBTTL
        ld      a,(MAXLNE)      ; else cause hof on record after title
        ld      (CURLNE),a
        ld      (NOLIST),a      ; suppress listing of 'TITLE'
        ret
;
;       PSECT
;

	psect	data

;
;	PSECT keywords table
;
;	Fixed PSECT names (4 chars, blanks appended) : text,data,bss
;	3 Custom PSECT names (4 chars, blanks appended)
;	PSECT flags: abs,pure,local,global,ovrld
;
PSECT_TAB:
	defb	4
	defm	'text'
	defb	0	;0=text
	defb	4
	defm	'data'
	defb	1	;1=data
	defb	4
	defm	'bss '
	defb	2	;2=bss
	defb	3
	defm	'abs'
	defb	3	;3=abs
	defb	4
	defm	'pure'
	defb	4	;4=pure
	defb	5
	defm	'local'
	defb	5	;5=local
	defb	6
	defm	'global'
	defb	6	;6=global
	defb	5
	defm	'ovrld'
	defb	7	;7=ovrld
	defb	0		;end mark / length
C1N:	defb	0,0,0,0		;place for CUST1 name
	defb	8	;8=cust1
	defb	0		;end mark / length
C2N:	defb	0,0,0,0		;place for CUST2 name
	defb	9	;9=cust2
	defb	0		;end mark / length
C3N:	defb	0,0,0,0		;place for CUST3 name
	defb	10	;10=cust3
	defb	0		;end mark
;
CUST_CNT:defb	0		;counter of custom segs
;
	psect	text
;
;	Mark syntax error
;
perr:
        ld      a,'L'
        ld      (ERRFLG),a
        ret
;
;	Get PSECT flags
;
;	A=delimiter
;
;	returns CARRY=0, B = flags
;			ABS 	= 80H (bit 7)
;			OVRLD 	= 40H (bit 6)
;			PURE 	= 20H (bit 5)
;			GLOBAL 	= 10H (bit 4)
;		else CARRY=1 : wrong flag
;
GetPsectFlags:
	ld	b,10H		;init B=flags (default=global)
looppf:
	cp	','
	scf
	ret	nz		;not the right delimiter
	ld	hl,(PTR1)	;skip delimiter
	inc	hl
	ld	(PTR1),hl
	push	bc		;save B
	call	ID		;get next id
	pop	bc		;restore B
	ld	c,a		;C=delimiter
        ld      a,(ERRFLG)
        cp      ' '
	scf
	ret	nz		;return if err
	push	bc		;save delimiter
	ld	de,PSECT_TAB
	ld	c,1
	call	SYMLUK
	pop	bc		;C=delimiter
	ret	c		;return if ID not found
	ld	a,(hl)		;get value
	cp	6		;global?
	jr	z,nextflag	;ignore (global is default, already set)
	cp	5		;local?
	jr	nz,seepure
				;is local
	res	4,b		;erase global flag
	jr	nextflag
seepure:
	cp	4		;pure?
	jr	nz,seeovrld
				;is pure
	set	5,b		;set pure flag
	jr	nextflag
seeovrld:
	cp	7		;ovrld?
	jr	nz,seeabs
				;is ovrld
	set	6,b		;set ovrld flag
	jr	nextflag
seeabs:
	cp	3		;abs?
	scf
	ret	nz		;return CARRY=1 if not
				;is abs
	set	7,b		;set abs flag
nextflag:			;try to process next
	ld	a,c		;check delimiter
	or	a		;EOL?
	ret	z		;return CARRY=0 if yes, B=flags
	jr	looppf
;
;	PSECT ENTRY
;
S670:
        call    ID              ;get psect name
	ld	c,a		;C=delimiter
        ld      a,(ERRFLG)
        cp      ' '
        ret     nz              ;on error, return
	push	bc		;save C=delimiter
	ld	a,(IDLEN)	;force seg name to 4 chars (pad with blanks)
	cp	4
	jr	z,is4
	jp	nc,perr		;accept only up to 4 chars as psect name
	ld	b,a		;B=len
	ld	hl,IDBUF	;adjust pointer to IDBUF
	ld	e,a
	ld	d,0
	add	hl,de
	ld	a,4
	sub	b
	ld	b,a		;remaining space to be filled with blanks
	ld	a,' '
loopf:	ld	(hl),a		;fill with ' '
	inc	hl
	djnz	loopf
	ld	a,4
	ld	(IDLEN),a
is4:				;search system seg names
	ld	de,PSECT_TAB
	ld	c,1
	call	SYMLUK
	pop	bc		;C=delimiter
	jp	c,custom
				;found,see which one it is
	ld	a,(hl)
	or	a
	jp	z,ptext		;TEXT(,ABS)
	cp	1
	jp	z,pdata		;DSEG
	cp	2
	jp	z,SBSS		;BSS
	cp	3		;'abs' not allowed as seg name
	jp	z,perr
	cp	4		;'pure' not allowed as seg name
	jp	z,perr
	cp	5		;'local' not allowed as seg name 
	jp	z,perr
	cp	6		;'global' not allowed as seg name 
	jp	z,perr
	cp	7		;'ovrld' not allowed as seg name 
	jp	z,perr
	cp	8
	jp	z,CST1
	cp	9
	jp	z,CST2
;				else is custom3
;	PSECT custom3
CST3:
	ld	a,1
	ld	(CUST3SEGUSED),a
	ld	a,c		;check delimiter
	or	a
	jp	z,CST3A		;CUST3
	call	GetPsectFlags	;identify psect flags
	jp	c,perr
	ld	a,b
	ld	(FLAG_C3),a	;save PSECT flags
CST3A:
        ld      a,(LOCFLG)
        or      a
        ld      a,(CURSEG)
        jr      nz,CST31
        cp      CUST3
        ret     z               ; current segment is CUST3, ignore
CST31:
        call    SAVEPC          ; save PC for current segment
        ld      hl,(CUST3SEGPC)
        ld      (PC),hl         ; and load PC with latest Data PC
        ld      e,CUST3         ; segment type = CUST3
        jp      S396            ; set segment type and loc counter
;
;	PSECT custom2
CST2:
	ld	a,1
	ld	(CUST2SEGUSED),a
	ld	a,c		;check delimiter
	or	a
	jp	z,CST2A		;CUST2
	call	GetPsectFlags	;identify psect flags
	jp	c,perr
	ld	a,b
	ld	(FLAG_C2),a	;save PSECT flags
CST2A:
        ld      a,(LOCFLG)
        or      a
        ld      a,(CURSEG)
        jr      nz,CST21
        cp      CUST2
        ret     z               ; current segment is CUST2, ignore
CST21:
        call    SAVEPC          ; save PC for current segment
        ld      hl,(CUST2SEGPC)
        ld      (PC),hl         ; and load PC with latest Data PC
        ld      e,CUST2         ; segment type = CUST2
        jp      S396            ; set segment type and loc counter
;
;	PSECT custom1
CST1:
	ld	a,1
	ld	(CUST1SEGUSED),a
	ld	a,c		;check delimiter
	or	a
	jp	z,CST1A		;CUST1
	call	GetPsectFlags	;identify psect flags
	jp	c,perr
	ld	a,b
	ld	(FLAG_C1),a	;save PSECT flags
CST1A:
        ld      a,(LOCFLG)
        or      a
        ld      a,(CURSEG)
        jr      nz,CST11
        cp      CUST1
        ret     z               ; current segment is CUST1, ignore
CST11:
        call    SAVEPC          ; save PC for current segment
        ld      hl,(CUST1SEGPC)
        ld      (PC),hl         ; and load PC with latest Data PC
        ld      e,CUST1         ; segment type = CUST1
        jp      S396            ; set segment type and loc counter
;
;	Process new custom segment request
;
custom:
	ld	a,(CUST_CNT)
	cp	3
	jp	z,perr		;only 3 custom segs allowed
	push	bc		;save C=delimiter
	inc	a		;increment custom segs counter
	ld	(CUST_CNT),a
				;place the length=4 and move the name to PSECT_TAB
	cp	1
	jr	nz,c2
				;CUST1
	ld	a,4		;set length
	ld	(C1N-1),a
	ld	hl,IDBUF	;move custom1 name
	ld	de,C1N
	ld	bc,4
	ldir
	pop	bc		;restore C=delimiter
	jp	CST1
c2:
	cp	2
	jr	nz,c3
				;CUST2
	ld	a,4		;set length
	ld	(C2N-1),a
	ld	hl,IDBUF	;move custom1 name
	ld	de,C2N
	ld	bc,4
	ldir
	pop	bc		;restore C=delimiter
	jp	CST2
c3:
				;CUST3
	ld	a,4		;set length
	ld	(C3N-1),a
	ld	hl,IDBUF	;move custom1 name
	ld	de,C3N
	ld	bc,4
	ldir
	pop	bc		;restore C=delimiter
	jp	CST3
;
;	TEXT	- must check for abs flag
;
ptext:
	ld	a,c		;check delimiter
	or	a
	jp	z,S390		;CSEG
	call	GetPsectFlags	;identify psect flags
	jp	c,perr
	bit	7,b		;abs == 80H
	jp	nz,S380		;if 'abs' was used, we have an ASEG
	ld	a,b
	ld	(FLAG_T),a	;save PSECT flags
;
;       CSEG
;
S390:
	ld	a,1
	ld	(CSEGUSED),a
        ld      a,(LOCFLG)
        or      a
        ld      a,(CURSEG)
        jr      nz,S390A
        cp      40h
        ret     z               ; current segment is Code, ignore
S390A:
        call    SAVEPC          ; save PC for current segment
        ld      hl,(CSEGPC)
        ld      (PC),hl         ; and load PC with latest Code PC
        ld      e,40h           ; segment type = Code
        jp      S396            ; set segment type and loc counter
;
;       ASEG			;PSECT TEXT,ABS == ASEG
;
S380:
	ld	a,1
	ld	(ASEGUSED),a
        xor     a
        dec     a
        ld      (LOCFLG),a      ; set loc pending flag
        ld      a,(CURSEG)
        or      a
        ret     z               ; current segment is Absolute, ignore
        call    SAVEPC          ; save PC for current segment
        ld      hl,(ASEGPC)
        ld      (PC),hl         ; and load PC with latest Absolute PC
        ld      a,00h
        ld      (CURSEG),a      ; set current segment type to Absolute
				;FLAG_A is already set as a constant = 0D0H
        ret
;
;	BSS
;
SBSS:
	ld	a,c		;check delimiter
	or	a
	jp	z,SBSSA		;BSS
	call	GetPsectFlags	;identify psect flags
	jp	c,perr
	ld	a,b
	ld	(FLAG_B),a	;save PSECT flags
SBSSA:
        ld      a,(LOCFLG)
        or      a
        ld      a,(CURSEG)
        jr      nz,S672
        cp      0C0h
        ret     z               ; current segment is BSS, ignore
S672:
        call    SAVEPC          ; save PC for current segment
        ld      hl,(BSEGPC)
        ld      (PC),hl         ; and load PC with latest BSS PC
        ld      e,0C0h          ; segment type = BSS
        jp      S396            ; set segment type and loc counter
;
;       DATA
;
pdata:
	ld	a,c		;check delimiter
	or	a
	jp	z,S391		;DSEG
	call	GetPsectFlags	;identify psect flags
	jp	c,perr
	ld	a,b
	ld	(FLAG_D),a	;save PSECT flags
;
;	DSEG
;
S391:
        ld      a,(LOCFLG)
        or      a
        ld      a,(CURSEG)
        jr      nz,S391A
        cp      80h
        ret     z               ; current segment is Data, ignore
S391A:
        call    SAVEPC          ; save PC for current segment
        ld      hl,(DSEGPC)
        ld      (PC),hl         ; and load PC with latest Data PC
        ld      e,80h           ; segment type = Data

S396:   ld      a,e
        ld      (CURSEG),a      ; set current segment type
        ld      a,(PASSNO)
        or      a
        call    nz,WLOC         ; if Pass 2, write loc counter 
        xor     a
        ld      (LOCFLG),a
        ret
;
;       COMMON
;
S392:   
;
;       GLOBAL                  ;ENTRY, GLOBAL, PUBLIC
;
S500:   
	call    ID              ; get label name
        ld      a,(ERRFLG)
        cp      ' '
        ret     nz              ; on error, return
        ld      hl,0
        ld      (VAL),hl
        ld      a,GBLSYM .or. UNDEF
        ld      (SYMMOD),a
        call    ADDSYM          ; enter symbol as Undefined PUBLIC label
        ret     c               ; on error, return
        inc     hl              ; skip over value
        inc     hl
        ld      a,(hl)          ; get mode bits
        jr      nz,S501         ; branch if new symbol
                                ; ... symbol already defined
        or      GBLSYM          ; set GLOBAL bit
        ld      (hl),a
        and     UNDEF           ; UNDEF bit set?
        jr      z,S502          ; branch if not
S501:                           ; symbol is new or has UNDEF flag set
        or      EXTSYM          ; ...so mark-it as external
        ld      (hl),a
;
        ld      a,1
        ld      (UFLAG),a       ; else set flag for LSTOUT
S502:   call    GNC
        cp      ','
        jr      z,S500          ; loop for more
        ret
;
;       EXT, EXTRN
;
S520:   ret
;
;       Add external symbol reference. Also called by CHKEXT from the
;       expression evaluation routine when a symbol of type LABEL## is
;       encountered.
;
ADDEXT: ld      hl,0
        ld      (VAL),hl
        ld      a,EXTSYM        ; set mode to External
        ld      (SYMMOD),a
        call    ADDSYM          ; enter symbol as External label
        ret     c               ; on error, return
        ret     nz              ; return if new symbol

        ld      a,(EVFLGS)      ; symbol exists, check flags
        and     0E0h
        cp      MULTDEF
        jp      z,MERROR        ; error if multiple defined

        cp      EQUNAME
        jp      z,SETMDF        ; type can't be EQU or DEFL
        cp      DFLNAME
        jp      z,SETMDF

        inc     hl              ; skip over value
        inc     hl
        ld      a,(hl)          ; get mode bits
        ld      c,a
        and     EXTSYM          ; External?
        ret     nz              ; return if yes
        ld      a,c
        and     GBLSYM .or. UNDEF
        cp      UNDEF           ; undefined?
        jp      nz,SETMDF       ; error if not
        or      EXTSYM          ; else set mode to External
        ld      (hl),a
        ret
;
;       .Z80, .Z180, .Z280
;
S530:   
;
;       .EVEN, .ODD
;
S540:   
;
;       NAME
;
S550:
;S552:   push    hl
;        inc     hl              ; point to name buffer
;        ld      c,0             ; init char count
;S553:   ld      a,(de)
;        call    UCASE
;        cp      ' '
;        jr      z,S554
;        ld      (hl),a          ; store name
;        inc     hl
;        inc     de
;        inc     c
;        ld      a,(NAMLEN)
;        cp      c
;        jr      nz,S553
;S554:   pop     hl
;        ld      (hl),c          ; store length of name
;        ret
;
;       IDENT
;
S560:   ret
;
;       INCLUDE, MACLIB
;
S570:   call    GNC             ; skip blanks
        ld      hl,(PTR1)
        dec     hl
        call    OPNLIB          ; open include file
        ld      (PTR1),hl
        ret
;
;       MACRO, REPT, IRP, IRPC
;
S580:   ld      hl,MTBL
        ld      a,(OPCODE+1)
        jp      SWITCH

	psect	data

MTBL:   defw    DEFMAC          ; MACRO
        defw    DFREPT          ; REPT
        defw    DFIRP           ; IRP
        defw    DFIRPC          ; IRPC

	psect	text

;
;       ENDM
;
S590:   jp      OERROR          ; ENDM without MACRO/REPT/IRP/IRPC
;
;       EXITM
;
S600:   ld      a,(MACLVL)
        or      a
        jp      nz,ENDMAC
        jp      OERROR          ; EXITM outside MACRO
;
;       LOCAL
;
S610:   jp      OERROR          ; LOCAL outside MACRO
;
;       RQST, .REQUEST
;
S620:  
;
;       .PHASE, .DEPHASE
;
S630:   
;
;       Get effective (.PHASE'd) PC value.
;
GETPPC:
        ld      hl,(PC)
	ret
;
;       .RADIX
;
S640:   ld      hl,10           ; argument to .RADIX is always in decimal,
        ld      (RADIX),hl      ;  regardless of the previous radix
        call    EVALNEW         ; get value
        ld      a,(EVMODE)
        call    CHK8U
        jp      nz,VALERR
        ld      a,l
        cp      2               ; only 2, 8, 10 and 16 allowed
        jr      z,S641
        cp      8
        jr      z,S641
        cp      10
        jr      z,S641
        cp      16
        jp      nz,VALERR
S641:   ld      (RADIX),hl
        ret
;
;       .PRINTX
;
S650:   ld      bc,RECMAX
        call    MCHECK          ; ensure there is enough memory for string
        jp      c,WERROR
        ld      de,(SYMPTR)
        inc     de              ; don't touch end of symbol table marker
        push    de
        call    S561            ; get string
        xor     a
        ld      (de),a
        pop     hl
        jp      CLINE           ; display line on console
;
S561:   call    GNC
        or      a
        ret     z
        cp      '('
        jr      nz,S563
        call    GNC
        cp      27H             ;' quote?
        jr      z,S562
        cp      '"'
        jp      nz,ERRQ
S562:   call    S563
        call    GNC
        cp      ')'
        jp      nz,ERRQ
        ret
;
S563:   ld      c,a             ; C = quote char
        ld      hl,(PTR1)
S564:   ld      a,(hl)
        or      a
        jr      z,S565
        inc     hl
        cp      c
        jr      z,S566
        ld      (de),a
        inc     de
        jr      S564
S565:   call    ERRQ            ; string not properly closed
S566:   ld      (PTR1),hl
        ret
;
;       .COMMENT
;
S660:   
;
S662:   ld      c,a
        ld      hl,(PTR1)
S663:   ld      a,(hl)          ; get char
        or      a               ; end of line?
        jr      z,S664          ; exit loop if yes
        inc     hl
        sub     c               ; delimiter found?
        jr      nz,S663         ; loop if not
        ld      (COMNTC),a      ; else clear flag/delimiter
S664:   ld      (PTR1),hl
        ret
;
;       Require char following call.
;       Set error flag otherwise and return with CY flag set.
;
REQCHR: call    GNC
        call    UCASE
        ex      (sp),hl
        cp      (hl)
        inc     hl
        ex      (sp),hl
        ret     z
OERROR: ld      a,'O'
        ld      (ERRFLG),a
        scf
        ret
;
;       Require signed 8-bit value in HL, A = mode.
;
REQ8S:  call    CHK8S
        ret     z
        jp      c,RELERR
VALERR: ld      a,'V'
        ld      (ERRFLG),a
        ret
;
;       Check if value in HL is signed 8-bit, A = mode.
;
CHK8S:  or      a
        scf
        ret     nz
        ld      a,l             ; check low byte
        or      a
        ld      a,h
        jp      m,CHKM
        or      a               ; if positive, high byte must be zero
        ret                     ; NZ means error
CHKM:   inc     a               ; if negative, high byte must be 0FFh
        ret                     ; NZ means error
;
;       Require unsigned 8-bit value in HL, A = mode.
;
REQ8U:  call    CHK8U
        ret     z
        jp      c,RELERR
        jr      VALERR
;
;       Check if value in HL is unsigned 8-bit, A = mode.
;
CHK8U:  or      a
        scf                     ; return with CY set if reloc value
        ret     nz
        ld      a,h             ; check high byte
        or      a
        ret     z               ; can be either 00h or 0FFh
        inc     a
        ret                     ; NZ means error
;
;       Save variables VAL, EVMODE, EXTCHN and CMNPTR to
;       SAVVAL, SAVMOD, SAVCHN and SAVCMN.
;
SAVVARS:push    bc
        push    de
        ld      hl,VAL
        ld      de,SAVVAL
        ld      bc,7
        ldir
        pop     de
        pop     bc
        ret
;
;       Save current segment PC
;
SAVEPC: ld      hl,(PC)
        ld      a,(CURSEG)
        cp      40h             ; Code segment?
        jr      z,SAVC
        cp      80h             ; Data segment?
        jr      z,SAVD
        cp      0C0h            ; BSS segment? ;COMMON
        jr      z,SAVB        
	cp	CUST1		; custom1?
	jr	z,SAVC1
	cp	CUST2		; custom2?
	jr	z,SAVC2
	cp	CUST3		; custom3?
	jr	z,SAVC3
	ld      (ASEGPC),hl     ; else is ASEG
        ret
SAVC1:	ld	(CUST1SEGPC),hl
	ret
SAVC2:	ld	(CUST2SEGPC),hl
	ret
SAVC3:	ld	(CUST3SEGPC),hl
	ret
SAVB: 	ld      (BSEGPC),hl
        ret
SAVD: 	ld      (DSEGPC),hl
        ret
SAVC:	ld      (CSEGPC),hl
        ret
;
;       Emit absolute byte in A to object file
;
EMITB:  push    hl
        ld      hl,PASSNO
        bit     0,(hl)
        jr      z,EMTB1         ; return if pass 1
        push    bc
        push    af
        call    LSTB            ; output to listing
        pop     af
        call    WOBJ            ; output to object file
        pop     bc
EMTB1:  pop     hl
        ld      a,(LEN)
        inc     a
        ld      (LEN),a
        ret
;
;       Emit absolute word in HL to object file
;
EMITW:  ld      a,l
        call    EMITB
        ld      a,h
        jp      EMITB
;
;       Emit word value in SAVVAL with segment type in SAVMOD to object file
;
EMITSV: ld      ix,SAVVAL       ; point to SAVVAL, SAVMOD, SAVCHN
        jr      EMTV0           ; continue below
;
;       Emit word value in VAL with segment type in EVMODE to object file
;
EMITV:  ld      ix,VAL          ; point to VAL, EVMODE, EXTCHN
EMTV0:  ld      a,(PASSNO)
        or      a
        jp      z,EMTV2         ; return if pass 1
EMTV1:  ld      a,(ix+2)        ; get mode
        bit     4,a             ; External?
        jr      z,EMTV3         ; branch if not
        push    bc
        ld      c,EXTSYM
        jr      EMTV6
EMTV3:  push    bc
        and     SEGMASK         ; mask segment bits
        ld      c,a
EMTV6:  ld      l,(ix+0)        ; get value into HL
        ld      h,(ix+1)
        push    hl
        call    WOBJ16          ; output relocatable value
        pop     hl
EMTV4:  ld      a,(ix+2)        ; get mode
        and     0E0H            ;0F0h           ; mask segment bits
        ld      c,a
        call    LSTW            ; output to listing
        pop     bc
EMTV2:  ld      a,(LEN)         ; update instr length
        inc     a
        inc     a
        ld      (LEN),a
        ret
;
;       Emit displacement SAVVAL-PC with segment type to object file.
;       Used by Z280 <addr> operands. On entry, DE contains an
;       additional offset to add to the displacement, as required
;       by some commands.
;
EMITSR:
       ld      ix,SAVVAL
       jr      EMTVR0
;
;       Emit displacement VAL-PC with segment type to object file.
;       Used by Z280 <addr> operands. On entry, DE contains an
;       additional offset to add to the displacement, as required
;       by some commands.
;
EMITVR:
       ld      ix,VAL
EMTVR0:
       ld      a,(PASSNO)
       or      a
       jr      z,EMTV2
       ld      l,(ix+0)        ; get value
       ld      h,(ix+1)
       dec     de              ; account for word length
       dec     de
       add     hl,de           ; add additional offset
       ld      de,(LEN)
       or      a
       sbc     hl,de
       ex      de,hl
       call    GETPPC          ; get effective PC value
       ex      de,hl           ;  into DE
       or      a
       sbc     hl,de           ; obtain relative displacement
       ld      (ix+0),l
       ld      (ix+1),h
       call    SUBVM           ; apply subtraction reloc rules
       jp      EMTV1
;
SUBVM:
       ld      a,(CURSEG)      ; check GETPPC
       and     11000000B       ; check current PC mode (2nd op)
       ret     z               ; return if Absolute
       ld      c,a
       ld      a,(ix+2)
       and     11000000B       ; else check VAL mode (1st op)
       cp      c
       jp      nz,RELERR       ; error if not same mode
       xor     a
       ld      (ix+2),a        ; else result just became Absolute
       ret
;
;	Check if more than one PSECT ASEG,CSEG,CUST1,CUST2,CUST3 type was used
;
;	return CARRY=1 if only one was used
;		else CARRY=0
;
CheckPSECTS:
	xor	a		;psect counter
	ld	b,5		;5 psects: ASEG,CSEG,CUST1,CUST2,CUST3
	ld	hl,ASEGUSED
ckps:	add	a,(hl)
	inc	hl
	djnz	ckps
	cp	2
	ret
;
;	Init JR pointers table
;	HL not affected
;
	global	InitJRtab
;
InitJRtab:
	push	hl
	ld	hl,JRTAB
	ld	(JRTABPS),hl	;init search pointer
	ld	(JRTABPA),hl	;init add pointer
	pop	hl
	ret
;
;	Add PC to JR pointers table
;
;	returns Z=1 if table full
;
AddToJRtab:
	ld	hl,(JRTABPA)
	ld	a,(hl)		;check for EOL (0FFFFH)
	inc	hl
	and	(hl)
	cp	0FFH
	ret	z		;full, quit
	dec	hl
	ld	de,(PC)
	ld	(hl),e		;store PC
	inc	hl
	ld	(hl),d
	inc	hl
	ld	(JRTABPA),hl	;save incremented add pointer
	ret
;
;	Search PC in JR pointers table
;
;	returns CARRY=0 if found, else CARRY=1
;
SearchJRtab:
	ld	bc,(PC)		;BC=PC
	ld	de,(JRTABPS)	;DE=search pointer
	ld	hl,(JRTABPA)	;HL=add pointer
	or	a
	sbc	hl,de		;if search pointer == add pointer...
	jr	z,notfound	;there are no more records
	ex	de,hl		;HL=search pointer
	ld	e,(hl)
	inc	hl
	ld	d,(hl)
	dec	hl		;DE=record,HL=search pointer
	ex	de,hl		;HL=record,DE=search pointer
	or	a
	sbc	hl,bc		;record == PC ?
	jr	z,found
notfound:
	scf
	ret			;return NOT FOUND
;
found:	inc	de		;increment seach pointer
	inc	de
	ld	(JRTABPS),de	;save search pointer
	or	a		;CARRY=0
	ret			;return FOUND
	
;	
	psect	data

;       SUBTTL  Opcode Table
;-----------------------------------------------------------------------
;
;       O P C O D E   T A B L E
;
;-----------------------------------------------------------------------
;
;       In order to enable binary searches, the opcode table has been
;       divided into several sections, each section containing entries
;       of equal length sorted alphabetically.

OPCODES:defw    OPCOD2,OPCOD3,OPCOD4,OPCOD5,OPCOD6,OPCOD7,OPCOD8
OPLEN:  defb    NUMOP2,NUMOP3,NUMOP4,NUMOP5,NUMOP6,NUMOP7,NUMOP8

OPCOD2: defb    2
        defm    'CP'
        defb    38h,0,6
        defb    2
        defm    'DB'
        defb    3,0,27
        defb    2
        defm    'DC'
        defb    3,1,27
        defb    2
        defm    'DI'
        defb    0F3h,0,24
        defb    2
        defm    'DS'
        defb    2,0,27
        defb    2
        defm    'DW'
        defb    4,0,27
        defb    2
        defm    'EI'
        defb    0FBh,0,24
        defb    2
        defm    'EX'
        defb    0EBh,0,10
        defb    2
        defm    'IF'
        defb    8,0,27
        defb    2
        defm    'IM'
        defb    0EDh,0FFh,1
        defb    2
        defm    'IN'
        defb    0,40h,7
        defb    2
        defm    'JP'
        defb    0C3h,0,3
        defb    2
        defm    'JR'
        defb    18h,0,4
        defb    2
        defm    'LD'
        defb    0,0,8
        defb    2
        defm    'OR'
        defb    30h,0,6
        defb    2
        defm    'RL'
        defb    10h,0,2
        defb    2
        defm    'RR'
        defb    18h,0,2
;        defb    2+Z280
;        defm    'SC'
;        defb    0EDh,71h,25
tmp2    equ     $ - OPCOD2
NUMOP2  equ     tmp2/6  ;(2+4)

OPCOD3: defb    3
        defm    'ADC'
        defb    8,4Ah,6
        defb    3
        defm    'ADD'
        defb    0,9,6
        defb    3
        defm    'AND'
        defb    20h,0,6
        defb    3
        defm    'BIT'
        defb    040h,0,12
        defb    3
        defm    'CCF'
        defb    03Fh,0,1
        defb    3
        defm    'CPD'
        defb    0EDh,0A9h,1
        defb    3
        defm    'CPI'
        defb    0EDh,0A1h,1
        defb    3
        defm    'CPL'
        defb    2Fh,0,23
;        defb    3+Z280
;        defm    'CPW'
;        defb    0C7h,0,15
        defb    3
        defm    'DAA'
        defb    27h,0,1
        defb    3
        defm    'DEC'
        defb    05h,0Bh,13
;        defb    3+Z280
;        defm    'DIV'
;        defb    0C4h,0,17
        defb    3
        defm    'END'
        defb    5,0,27
        defb    3
        defm    'EQU'
        defb    1,0,27
        defb    3
        defm    'EXT'
        defb    18,0,27
        defb    3
        defm    'EXX'
        defb    0D9h,0,1
        defb    3
        defm    'IF1'
        defb    8,1,27
        defb    3
        defm    'IF2'
        defb    8,2,27
        defb    3
        defm    'IFB'
        defb    8,7,27
        defb    3
        defm    'IFF'
        defb    8,4,27
        defb    3
        defm    'IFT'
        defb    8,3,27
        defb    3+Z180
        defm    'IN0'
        defb    2,0,7
        defb    3
        defm    'INC'
        defb    04h,03h,13
        defb    3
        defm    'IND'
        defb    0EDh,0AAh,1
        defb    3
        defm    'INI'
        defb    0EDh,0A2h,1
;        defb    3+Z280
;        defm    'INW'
;        defb    6,0,7
        defb    3
        defm    'IRP'
        defb    24,2,27
;        defb    3+Z280
;        defm    'JAF'
;        defb    28h,0DDh,4
;        defb    3+Z280
;        defm    'JAR'
;        defb    20h,0DDh,4
;        defb    3+Z280
;        defm    'LDA'
;        defb    0,0,19
        defb    3
        defm    'LDD'
        defb    0EDh,0A8h,1
        defb    3
        defm    'LDI'
        defb    0EDh,0A0h,1
;        defb    3+Z280
;        defm    'LDW'
;        defb    1,0,8
;        defb    3+Z180
;        defm    'MLT'
;        defb    0,0,14
        defb    3
        defm    'NEG'
        defb    0EDh,044h,23
        defb    3
        defm    'NOP'
        defb    0,0,1
        defb    3
        defm    'ORG'
        defb    6,0,27
        defb    3
        defm    'OUT'
        defb    1,41h,7
        defb    3
        defm    'POP'
        defb    0C1h,0,9
        defb    3
        defm    'RES'
        defb    80h,0,12
        defb    3
        defm    'RET'
        defb    0C9h,0,11
        defb    3
        defm    'RLA'
        defb    17h,0,1
        defb    3
        defm    'RLC'
        defb    0,0,2
        defb    3
        defm    'RLD'
        defb    0EDh,06Fh,1
        defb    3
        defm    'RRA'
        defb    1Fh,0,1
        defb    3
        defm    'RRC'
        defb    8,0,2
        defb    3
        defm    'RRD'
        defb    0EDh,067h,1
        defb    3
        defm    'RST'
        defb    0C7h,0,5
        defb    3
        defm    'SBC'
        defb    18h,42h,6
        defb    3
        defm    'SCF'
        defb    037h,0,1
        defb    3
        defm    'SET'
        defb    0C0h,0,12
        defb    3
        defm    'SLA'
        defb    20h,0,2
        defb    3
        defm    'SLL'
        defb    20h,0,2
;        defb    3+Z180
;        defm    'SLP'
;        defb    0EDh,76h,1
        defb    3
        defm    'SRA'
        defb    28h,0,2
        defb    3
        defm    'SRL'
        defb    38h,0,2
        defb    3
        defm    'SUB'
        defb    10h,0,6
;        defb    3+Z180
;        defm    'TST'
;        defb    1,0,14
        defb    3
        defm    'XOR'
        defb    28h,0,6
tmp3    equ     $ - OPCOD3
NUMOP3  equ     tmp3/7  ;(3+4)

OPCOD4: 
;	defb    4
;        defm    '.ODD'
;        defb    20,1,27
;        defb    4
;        defm    '.Z80'
;        defb    19,0,27
;        defb    4+Z280
;        defm    'ADDW'
;        defb    0C6h,0,15
        defb    4
        defm    'ASEG'
        defb    15,0,27
;        defb    4
;        defm    'ASET'
;        defb    1,1,27
        defb    4
        defm    'CALL'
        defb    0CDh,0,3
        defb    4
        defm    'COND'
        defb    8,0,27
        defb    4
        defm    'CPDR'
        defb    0EDh,0B9h,1
        defb    4
        defm    'CPIR'
        defb    0EDh,0B1h,1
        defb    4
        defm    'CSEG'
        defb    13,0,27
;        defb    4+Z280
;        defm    'DECW'
;        defb    0,0Bh,16
        defb    4
        defm    'DEFB'
        defb    3,0,27
        defb    4
        defm    'DEFC'
        defb    3,1,27
;        defb    4
;        defm    'DEFL'
;        defb    1,1,27
        defb    4
        defm    'DEFF'
        defb    34,0,27
        defb    4
        defm    'DEFM'
        defb    3,0,27
        defb    4
        defm    'DEFS'
        defb    2,0,27
        defb    4
        defm    'DEFW'
        defb    4,0,27
        defb    4
        defm    'DEFZ'
        defb    3,2,27
;        defb    4+Z280
;        defm    'DIVU'
;        defb    0C5h,0,17
;        defb    4+Z280
;        defm    'DIVW'
;        defb    0CAh,0,18
        defb    4
        defm    'DJNZ'
        defb    10h,0,4
        defb    4
        defm    'DSEG'
        defb    14,0,27
        defb    4
        defm    'ELSE'
        defb    9,0,27
        defb    4
        defm    'ENDC'
        defb    10,0,27
        defb    4
        defm    'ENDM'
        defb    25,0,27
;        defb    4+Z280
;        defm    'EPUF'
;        defb    0EDh,97h,1
;        defb    4+Z280
;        defm    'EPUI'
;        defb    0EDh,9Fh,1
;        defb    4+Z280
;        defm    'EPUM'
;        defb    0EDh,84h,26
;        defb    4+Z280
;        defm    'EXTS'
;        defb    0EDh,64h,23
        defb    4
        defm    'FORM'
        defb    7,0,27
        defb    4
        defm    'HALT'
        defb    76h,0,1
        defb    4
        defm    'IFNB'
        defb    8,8,27
;        defb    4+Z280
;        defm    'INCW'
;        defb    0,03h,16
        defb    4
        defm    'INDR'
        defb    0EDh,0BAh,1
        defb    4+Z280
        defm    'INDW'
        defb    0EDh,8Ah,1
        defb    4
        defm    'INIR'
        defb    0EDh,0B2h,1
;        defb    4+Z280
;        defm    'INIW'
;        defb    0EDh,82h,1
        defb    4
        defm    'IRPC'
        defb    24,3,27
        defb    4
        defm    'JOPT'
        defb    35,0,27
        defb    4
        defm    'LDDR'
        defb    0EDh,0B8h,1
        defb    4
        defm    'LDIR'
        defb    0EDh,0B0h,1
;        defb    4+Z280
;        defm    'LDUD'
;        defb    86h,0,21
;        defb    4+Z280
;        defm    'LDUP'
;        defb    96h,0,21
        defb    4
        defm    'LIST'
        defb    11,0,27
;        defb    4+Z280
;        defm    'MEPU'
;        defb    0EDh,85h,26
;        defb    4+Z280
;        defm    'MULT'
;        defb    0C0h,0,22
;        defb    4
;        defm    'NAME'
;        defb    21,0,27
;        defb    4+Z180
;        defm    'OTDM'
;        defb    0EDh,8Bh,1
        defb    4
        defm    'OTDR'
        defb    0EDh,0BBh,1
;        defb    4+Z180
;        defm    'OTIM'
;        defb    0EDh,83h,1
        defb    4
        defm    'OTIR'
        defb    0EDh,0B3h,1
;        defb    4+Z180
;        defm    'OUT0'
;        defb    3,0,7
        defb    4
        defm    'OUTD'
        defb    0EDh,0ABh,1
        defb    4
        defm    'OUTI'
        defb    0EDh,0A3h,1
;        defb    4+Z280
;        defm    'OUTW'
;        defb    7,0,7
        defb    4
        defm    'PAGE'
        defb    7,1,27
        defb    4
        defm    'PUSH'
        defb    0C5h,0,9
        defb    4
        defm    'REPT'
        defb    24,1,27
        defb    4
        defm    'RETI'
        defb    0EDh,4Dh,1
        defb    4
        defm    'RETN'
        defb    0EDh,45h,1
        defb    4
        defm    'RLCA'
        defb    07h,0,1
;        defb    4
;        defm    'RQST'
;        defb    28,0,27
        defb    4
        defm    'RRCA'
        defb    0Fh,0,1
;        defb    4+Z280
;        defm    'SUBW'
;        defb    0CEh,0,15
;        defb    4+Z280
;        defm    'TSET'
;        defb    30h,0,2
;        defb    4+Z280
;        defm    'TSTI'
;        defb    5,0,7
tmp4    equ     $ - OPCOD4
NUMOP4  equ     tmp4/8  ;(4+4)

OPCOD5: 
;	defb    5
;        defm    '.EVEN'
;        defb    20,0,27
        defb    5
        defm    '$LALL'
        defb    11,9,27
        defb    5
        defm    '$LIST'
        defb    11,1,27
        defb    5
        defm    '$SALL'
        defb    11,11,27
        defb    5
        defm    '$XALL'
        defb    11,10,27
        defb    5
        defm    '*LIST'
        defb    11,0,27
;        defb    5
;        defm    '.Z180'
;        defb    19,1,27
;        defb    5
;        defm    '.Z280'
;        defb    19,2,27
;        defb    5+Z280
;        defm    'DIVUW'
;        defb    0CBh,0,18
        defb    5
        defm    'EJECT'
        defb    7,0,27
        defb    5
        defm    'ENDIF'
        defb    10,0,27
;        defb    5
;        defm    'ENTRY'
;        defb    17,0,27
        defb    5
        defm    'EXITM'
        defb    26,0,27
;        defb    5
;        defm    'EXTRN'
;        defb    18,0,27
        defb    5
        defm    'IDENT'
        defb    22,0,27
        defb    5
        defm    'IFDEF'
        defb    8,5,27
        defb    5
        defm    'IFDIF'
        defb    8,10,27
        defb    5
        defm    'IFIDN'
        defb    8,9,27
;        defb    5
;        defm    'IFZ80'
;        defb    8,11,27
;        defb    5+Z280
;        defm    'INDRW'
;        defb    0EDh,9Ah,1
;        defb    5+Z280
;        defm    'INIRW'
;        defb    0EDh,92h,1
;        defb    5+Z280
;        defm    'LDCTL'
;        defb    0,0,20
        defb    5
        defm    'LOCAL'
        defb    27,0,27
        defb    5
        defm    'MACRO'
        defb    24,0,27
;        defb    5+Z280
;        defm    'MULTU'
;        defb    0C1h,0,22
;        defb    5+Z280
;        defm    'MULTW'
;        defb    0C2h,0,15
;        defb    5+Z180
;        defm    'OTDMR'
;        defb    0EDh,9Bh,1
;        defb    5+Z280
;        defm    'OTDRW'
;        defb    0EDh,9Bh,1
;        defb    5+Z180
;        defm    'OTIMR'
;        defb    0EDh,93h,1
;        defb    5+Z280
;        defm    'OTIRW'
;        defb    0EDh,93h,1
;        defb    5+Z280
;        defm    'OUTDW'
;        defb    0EDh,8Bh,1
;        defb    5+Z280
;        defm    'OUTIW'
;        defb    0EDh,83h,1
        defb    5
        defm    'PSECT'
        defb    33,0,27
;        defb    5+Z280
;        defm    'RETIL'
;        defb    0EDh,55h,1
        defb    5
        defm    'TITLE'
        defb    12,0,27
;        defb    5+Z180
;        defm    'TSTIO'
;        defb    4,0,7
tmp5    equ     $ - OPCOD5
NUMOP5  equ     tmp5/9  ;(5+4)

OPCOD6: 
;	defb    6
;        defm    '.PHASE'
;        defb    29,0,27
        defb    6
        defm    '$RADIX'
        defb    30,0,27
        defb    6
        defm    '$XLIST'
        defb    11,2,27
        defb    6
        defm    '*EJECT'
        defb    7,0,27
        defb    6
        defm    '*TITLE'
        defb    12,0,27
;        defb    6
;        defm    'COMMON'
;        defb    16,0,27
        defb    6
        defm    'GLOBAL'
        defb    17,0,27
        defb    6
        defm    'IFNDEF'
        defb    8,6,27
;        defb    6
;        defm    'IFZ180'
;        defb    8,12,27
;        defb    6
;        defm    'IFZ280'
;        defb    8,13,27
;        defb    6
;        defm    'MACLIB'
;        defb    23,1,27
;        defb    6+Z280
;        defm    'MULTUW'
;        defb    0C3h,0,15
;        defb    6+Z280
;        defm    'PCACHE'
;        defb    0EDh,65h,1
;        defb    6
;        defm    'PUBLIC'
;        defb    17,0,27
        defb    6
        defm    'SUBTTL'
        defb    12,1,27
tmp6    equ     $ - OPCOD6
NUMOP6  equ     tmp6/10 ;(6+4)

OPCOD7: defb    7
        defm    '$LFCOND'
        defb    11,3,27
        defb    7
        defm    '$PRINTX'
        defb    31,0,27
        defb    7
        defm    '$SFCOND'
        defb    11,4,27
        defb    7
        defm    '*AM9511'
        defb    36,0,27
        defb    7
        defm    'INCLUDE'
        defb    23,0,27
tmp7    equ     $ - OPCOD7
NUMOP7  equ     tmp7/11 ;(7+4)

OPCOD8: 
        defb    8
        defm    '*HEADING'
        defb    12,1,27
        defb    8
        defm    '*INCLUDE'
        defb    23,0,27
;	 defb    8
;        defm    '.COMMENT'
;        defb    32,0,27
;        defb    8
;        defm    '.DEPHASE'
;        defb    29,1,27
;        defb    8
;        defm    '.REQUEST'
;        defb    28,0,27
tmp8    equ     $ - OPCOD8
NUMOP8  equ     tmp8/12 ;(8+4)

        defb    0

;-----------------------------------------------------------------------
;               COMMON DATA AREA
;-----------------------------------------------------------------------

HOFMSG: defb    0Ch
        defm    'Z80AS '
        defb    VER1
        defm    '.'
        defb    VER2
        defb    TAB
        defm    'Source file: '
HOFNAM: defm    '         '
        defb    TAB
        defm    'Page '
HOFPG:  defm    '    '
HOFEND: defb    0

HDRBUF: defs    HDRSZ   ; line header buffer
REC:    defs    RECMAX  ; input line, must follow HDRBUF

TITLEB: defs    81      ; title buffer (80 chars + trailing null)
SBTTLB: defs    61      ; subtitle buffer (60 chars + trailing null)

;
;	JP optimization table
;
JRTAB:			; space for 1024 JR pointers = 8 groups x 8 lines x 16 words
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	;1
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ;2
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0	;3
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ;4
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ;5
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ;6
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ;7
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0 ;8
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0
	defw	0,0,0,0,0,0,0,0,0,0,0,0,0,0,0,0

	defw	0FFFFH	;EOL
JRTABPS:defs	2	; search pointer
JRTABPA:defs	2	; add pointer
;
IDLEN:  defb    0       ; length of identifier
IDBUF:  defs    IDMAX   ; current identifier

;NAMLEN: defb    6       ; max REL symbol length (5..8)

;MODNAM: defb    0       ; length
;        defs    8       ; module name
;MODIDN: defb    0       ; length
;        defs    8       ; module ID

AM9511F:defb	0	; AM9511 flag (0 = OFF)
CPU:    defb    0       ; target CPU type: 0=Z80, 1=Z180, 2=Z280
DEFCPU: defb    0       ; default CPU type from command line
PC:     defw    0       ; current program counter
;
;	order is critical - do not move ---------
;
ASEGUSED:defb	0
CSEGUSED:defb	0
CUST1SEGUSED:defb 0
CUST2SEGUSED:defb 0
CUST3SEGUSED:defb 0
;
;------------------------------------------------
;
ASEGPC: defw    0       ; current absolute segment counter
CSEGPC: defw    0       ; current code segment counter
CUST1SEGPC:defw	0	; current custom1 segment counter
CUST2SEGPC:defw	0	; current custom2 segment counter
CUST3SEGPC:defw	0	; current custom3 segment counter
DSEGPC: defw    0       ; current data segment counter
BSEGPC: defw    0       ; current bss segment counter

;PSECT flag =
;(if local) 
;	(if ovrld) 40H + (if abs) 80H + (if pure) 20H
;else (global is default)
;	10H + (if ovrld) 40H + (if abs) 80H + (if pure) 20H

;FLAG_A=0D0H (ovrld+abs+global) ASEG PSECT FLAG
FLAG_T:	defb	10H	;TEXT PSECT flag (global is default)
FLAG_D:	defb	10H	;DATA PSECT flag (global is default)
FLAG_B:	defb	10H	;BSS PSECT flag (global is default)
FLAG_C1:defb	10H	;CUST1 PSECT flag (global is default)
FLAG_C2:defb	10H	;CUST2 PSECT flag (global is default)
FLAG_C3:defb	10H	;CUST3 PSECT flag (global is default)

LEN:    defb    0       ; length of current instruction
LENDS:  defw    0       ; for DEFS
CURSEG: defb    0       ; current segment: 40h=TEXT, 80h=DATA, C0h=BSS
			;	2=custom1, 4=custom2, 8=custom3
PTR1:   defw    0       ; points to next char in REC
CURLNE: defb    0       ; current line number for paging output
EQUFLG: defb    0       ; if non-zero VAL is used instead of PC for print
LBLFLG: defb    0       ; if non-zero, force PC output to listing
DSFLAG: defb    0       ; if non-zero LENDS is used for print
DBWFLG: defb    0       ; DB/DC/DW flag
LOCFLG: defb    0       ; if non-zero, loc counter is pending output
NEWSYM: defb    0       ; new symbol flag
ENDADR: defw    0       ; expression value on END statement
ENDMOD: defb    0       ; expression result mode on END statement
ENDMARK:defb	0	; 0 if no END start
EFLG:   defb    0       ; end of program flag (to allow printing of END stmt)
OPCODE: defw    0       ; current opcode from symbol table
RADIX:  defw    0       ; default radix for numeric conversion
COMNTC: defb    0       ; .COMMENT delimiter char

VAL:    defw    0       ; return from EVAL routine      !   do   !
EVMODE: defb    0       ; expression result mode        !  not   !
EXTCHN: defw    0       ; External chain address        ! change !
CMNPTR: defw    0       ; pointer to COMMON segment     ! order  !

SAVVAL: defw    0       ; saved contents of VAL         !   do   !
SAVMOD: defb    0       ; saved contents of EVMODE      !  not   !
SAVCHN: defw    0       ; saved contents of EXTCHN      ! change !
SAVCMN: defw    0       ; saved contents of CMNPTR      ! order  !

LPFLAG: defb    0       ; listing line pending flag
LSTCNT: defb    0       ; character count of object code field in listing
LFLAG:  defb    0       ; listing flag:'A-D' = PRN file destn drive
                        ; 'Z' = no listing; 'X' = listing to screen
                        ; 'Y' = listing to screen, errors echoed to printer
                        ; 'P' = listing to printer
OFLAG:  defb    0       ; object flag: 'Z' = no obj, 'A-D' = REL file destn drive
QFLAG:  defb    0       ; quiet flag
XFLAG:	defb	0	; global/external symbols flag
DFLAG:	defb	0	; DEFS init flag

;order of next 4 bytes is mandatory!
JOPTDIS:defb	0FFH	; 0 = jump optimization disabled
JFLAG:	defb	0	; if 0FFH, check all JP ranges
JPASS:	defb	0	; extra pass marker (0FFH = ON)
PASSNO: defb    0       ; current pass: 0=pass 1, FF=pass 2
;
JCOUNT:	defw	0	; jump optimization counter
UMODE:  defb    0       ; if set, treat all undefined symbols as externals
ERRFLG: defb    0       ; error character for this line
ERRCNT: defw    0       ; error count
MACFLG: defb    0       ; MACRO expansion flag for listing
SYMPTR: defw    0       ; address of next symbol table entry
MAXMEM: defw    0       ; maximum usable memory address
DSPTR:  defw    0       ; pointer to start of dynamic storage
PCFLAG: defb    0       ; PC relative value in EVAL
UFLAG:  defb    0       ; undefined flag from EVAL, 0 = all ok, 1 or >1 = undefined
EVFLGS: defb    0       ; flag field from last SYMLUK
LSTOPT: defb    0       ; list options
IFLIST: defb    0       ; set true to suppress listing on IF, ELSE, ENDIF
                        ;  when "LIST NOCOND" is current.
NOLIST: defb    0       ; set to true to avoid listing TITLE, FORM, PAGE, EJECT

CONDSP: defw    CNDSTK  ; conditionals stack pointer
        defs    CSTKSZ  ; conditionals stack
CNDSTK: defb    0FFh    ; we always start true
CLEVEL: defb    0       ; conditionals stack level

SYMMOD: defb    0       ; symbol address mode
SYMADR: defw    0       ; address of data field for last SYMENT
IDADR:  defw    0       ; address of ID field for last SYMENT

SYMTBL: defw    0       ; address of first available sym table slot
