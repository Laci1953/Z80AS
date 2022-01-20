;	Copyright 2022 Ladislau Szilagyi
;
;	Object code builder for ZSM assembler
;

*include	ZSM.INC

	psect	text

	global	WLOC,WOBJ,WOBJ16,WDFENT
	global	INIOBJ,CLSOBJ,IDBUF,IDLEN
;extern
	global	OFLAG,CLOSE2,WNB2,BSSIZE
	global	SYMTBL,SYMLUK
	global	LASTEXTSYM
	global	ENDADR,ENDMOD,ENDMARK
;
NOOBJ	equ	0
DEBUG	equ	0
;----------------------------------------------------------------------------
COND	DEBUG
;
;       Type String
;
;       Print string (zero terminated)
;
;       HL=string addr
;       BC,DE not affected
;
TypeString:
        ld      a,(hl)
        or      a
        ret     z
        out	(1),a
        inc     hl
        jr      TypeString
;
;       Byte To Nibbles
;
;       Convert byte to nibbles
;       A = Hex byte
;       returns D = Most significant nibble, E = Least significant nibble
;       registers not affected (except AF)
;
ByteToNibbles:
        ld      e,a
        rra
        rra
        rra
        rra
        and     0FH
        ld      d,a
        ld      a,e
        and     0FH
        ld      e,a
        ret
;
;       Converts Nibble A to ASCII
;
;       Converts Nibble (0-15) to its ASCII value ('0' to '9', or 'A' to 'F')
;
;       A=Nibble
;       returns A=ASCII value of byte (letters in uppercase)
;       registers not affected (except AF)
;
NibbleToASCII:
        cp      10              ;digit?
        jr      nc,nodig
        add     a,'0'           ;it's a digit
        ret
nodig:  add     a,'A'-10        ;no, it's a letter (A to F)
        ret
;
;       Print a byte in A (2 hexa chars, uppercase)
;
;       A=byte
;	BC,DE,HL not affected
;
TypeA:
	push	de
        call    ByteToNibbles   ;High Nibble = D, Low Nibble = E
        ld      a,d
        call    NibbleToASCII            
        out	(1),a		;type High Nibble to console
        ld      a,e
        call    NibbleToASCII
        out	(1),a		;type Low Nibble to console
	pop	de
        ret
;
WOBJ_t:defb 0dh,0ah	
	defm	'WOBJ '
	defb 0
WOBJ16_t:defb 0dh,0ah	
	defm	'WOBJ16 '
	defb 0
WLOC_t:defb 0dh,0ah	
	defm	'WLOC '
	defb 0
WDFENT_t:defb 0dh,0ah	
	defm	'WDFENT '
	defb 0
HL_t:	defm	' HL='
	defb	0
DE_t:	defm	' DE='
	defb	0
BC_t:	defm	' BC='
	defb	0
H_t:	defm	' H='
	defb	0
L_t:	defm	' L='
	defb	0
D_t:	defm	' D='
	defb	0
E_t:	defm	' E='
	defb	0
B_t:	defm	' B='
	defb	0
C_t:	defm	' C='
	defb	0
A_t:	defm	' A='
	defb	0
ENDC
;----------------------------------------------------------------------------
;
;	WOBJ - Write absolute byte in A
;
WOBJ:
COND	NOOBJ
	ret
ENDC	
COND	DEBUG
	push	hl
	push	af
	ld	hl,WOBJ_t
	call	TypeString
	ld	hl,A_t
	call	TypeString
	pop	af
	call	TypeA
	pop	hl
	ret
ENDC
COND	1-DEBUG
	push	af
	ld	a,(PBUF)
	or	a		;if buffer empty
	call	z,SetTextType	;then prepare buffer
	ld	a,(PBUF)
	cp	SAFE		;if safe limit reached
	jr	c,ok1
	call	WriteBuf	;then write buffer
	call	SetTextType	;prepare a new one
	call	DeployREL	;then write RELOC recs (if any...)
ok1:	pop	af		;A=byte
	call	StoreByte	;store-it
	ld	hl,(PC)		;increment PC
	inc	hl
	ld	(PC),hl
	ret
ENDC
;
;	WBOJ16 - Write relative word in HL, C = seg type.
;	C=	40H for TEXT, 
;		80H for DATA,
;		C0H for BSS, 
;		00H for ASEG(TEXT,ABS) (not anymore relative!),
;		10H for external reference (symbol in (LASTEXTSYM))
;	
WOBJ16:	
COND	NOOBJ
	ret
ENDC	
COND	DEBUG
	push	hl
	ld	hl,WOBJ16_t
	call	TypeString
	ld	hl,HL_t
	call	TypeString
	pop	hl
	push	hl
	ld	a,h
	call	TypeA
	ld	a,l
	call	TypeA
	ld	hl,C_t
	call	TypeString
	ld	a,c
	call	TypeA
	ld	a,c
	cp	10h
	jr	nz,g1
	ld	a,' '
	out	(1),a
	ld	hl,(LASTEXTSYM)
	ld	a,(hl)
	ld	b,a
	inc	hl
l2:	ld	a,(hl)
	out	(1),a
	inc	hl
	djnz	l2
g1:
	pop	hl
	ret
ENDC
COND	1-DEBUG
				;first store word in TEXT buffer 
	push	hl		;save word
	push	bc		;save seg type
	ld	a,(PBUF)
	or	a		;if buffer empty
	call	z,SetTextType	;then prepare buffer
	ld	a,(PBUF)
	cp	SAFE		;if safe limit reached
	jr	c,ok2
	call	WriteBuf	;then write buffer
	call	SetTextType	;prepare a new one
	call	DeployREL	;then write RELOC recs (if any...)
ok2:	pop	de		;restore E=seg type
	pop	bc		;restore BC=word
	call	StoreWord
;
	ld	a,e		;check seg type
	or	a		;seg is ASEG?
	jr	z,INC_PC	;if yes, just increment PC

				;E=seg type, (PC)=offset
	cp	10H		;external ref?
	jr	nz,ok3		
	ld	bc,(LASTEXTSYM)	;yes, symbol in (LASTEXTSYM)
ok3:
	call	StoreREL	;store RELOC record
INC_PC:				;PC=PC+2
	ld	hl,(PC)
	inc	hl
	inc	hl
	ld	(PC),hl
	ret
ENDC
;
;	WLOC - Write loc counter: HL = address, E = seg type (40h = CSEG,
;	80h = DSEG, 0C0h = BSS, 00 = ASEG)
;
WLOC:	
COND	NOOBJ
	ret
ENDC	
COND	DEBUG
	push	hl
	ld	hl,WLOC_t
	call	TypeString
	ld	hl,HL_t
	call	TypeString
	pop	hl
	push	hl
	ld	a,h
	call	TypeA
	ld	a,l
	call	TypeA
	ld	hl,E_t
	call	TypeString
	ld	a,e
	call	TypeA
	pop	hl
	ret
ENDC
	ld	(PC),hl		;save PC
	push	de
	ld	a,(PBUF)	;buffer loaded?
	or	a
	jr	z,e1
	call	WriteBuf	;yes, then write the buffer
	call	DeployREL	;then write RELOC recs (if any...)
e1:
	ld	a,(RPBUF)	;buffer loaded?
	or	a
	call	nz,WriteRBuf	;yes, then save the buffer
	pop	de
	ld	a,e
	ld	(CRTGRP),a	;save new group
	cp	40H		;CSEG?
	jr	nz,e2
	ld	hl,CSEG_F
	jr	e5
e2:	cp	80H		;DSEG?
	jr	nz,e3
	ld	hl,DSEG_F
	jr	e5
e3:	cp	0C0H		;BSEG?
	jr	nz,e4
	ld	hl,BSEG_F
	jr	e5
e4:	ld	hl,ASEG_F	;else it is an ASEG
e5:	ld	(hl),1		;set the flag
	ret
;
;	WDFENT - Write entry point record: DE = address, C = seg type,
;	HL = name, B = length
;
WDFENT:	
COND	NOOBJ
	ret
ENDC	
COND	DEBUG
	push	hl
	ld	hl,WDFENT_t
	call	TypeString
	ld	hl,HL_t
	call	TypeString
	pop	hl
	push	hl
	ld	a,h
	call	TypeA
	ld	a,l
	call	TypeA
	ld	hl,DE_t
	call	TypeString
	ld	a,d
	call	TypeA
	ld	a,e
	call	TypeA
	ld	hl,B_t
	call	TypeString
	ld	a,b
	call	TypeA
	ld	hl,C_t
	call	TypeString
	ld	a,c
	call	TypeA
	ld	a,' '
	out	(1),a
	pop	hl
	push	hl
l3:	ld	a,(hl)
	inc	hl
	out	(1),a
	djnz	l3
	pop	hl
	ret
ENDC
COND	1-DEBUG
	push	bc
	push	de
	push	hl	

	ld	a,(PSECT_DONE)	;PSECT records were written?
	or	a
	jp	nz,symbol
				;no, write them...
	call	WritePSECTS
	call	SetSymType	;now setup buffer for SYM
				;and write symbol record...
symbol:
	ld	a,(PBUF)	;if no more space available
	cp	RSAFE
	jr	c,w5
	call	WriteBuf	;first write buffer
	call	SetSymType	;and setup buffer for SYM
w5:
	pop	hl
	pop	de
	pop	bc
	
	jp	WriteSym
;
ENDC
;
;	INIOBJ - initialize OBJ
;	write IDENT rec
;	reset vars
;
INIOBJ:	
COND	NOOBJ
	ret
ENDC	
	xor	a
	ld	(CRTGRP),a	;reset crt recs group
	ld	(ASEG_F),a	;reset segment flags
	ld	(CSEG_F),a
	ld	(DSEG_F),a
	ld	(BSEG_F),a
	ld	(PSECT_DONE),a	;reset PSECT flag

	ld	hl,BIGBUF	;set buffers pointers
	inc	h
	ld	l,a		;HL = xx00
	ld	(BUFFER),hl
	ld	(PBUF),hl
	inc	h
	ld	(RBUFFER),hl
	ld	(RPBUF),hl

	dec	a		;A=FFH
	ld	hl,RELBUF
	ld	(hl),a		;init RELBUF with EOL mark

	ld	hl,ID_REC	;write IDENT record
	ld	b,ID_REC_LEN
	jp	WriteBytes
;
;	CLSOBJ - Write end record
;	close object file.
;
CLSOBJ:	
COND	NOOBJ
	jp	CLOSE2		; close REL file
ENDC	
	ld	a,(PBUF)	;buffer loaded?
	or	a
	jr	z,c1
	call	WriteBuf	;yes, then write the buffer
	call	DeployREL	;then write RELOC recs (if any...)
c1:
	ld	a,(RPBUF)	;buffer loaded?
	or	a
	call	nz,WriteRBuf	;yes, then save the buffer

	ld	a,(PSECT_DONE)	;PSECT records were written?
	or	a		;if no, write them...
	call	z,WritePSECTS

	ld	a,(ENDMARK)	;END addr was specified?
	or	a
	jr	z,wend
				;then write the START record
	ld	hl,(ENDADR) 
	ld	(start),hl
;	ld	a,(ENDMOD)
;	cp	40H		;start in psect text?
;	ld	b,START_REC_LEN
;	jr	z,wstart
;				;else is in ASEG (psect text,abs)
;	ld	a,5
;	ld	(START_REC),a	;4+1
;	xor	a
;	ld	(pstart),a	;'' as psect
;	ld	b,START_REC_LEN-4
;wstart:
	ld	hl,START_REC
	ld	b,START_REC_LEN
	call	WriteBytes
wend:				;write END record
	ld	hl,END_REC	;BSS
	ld	b,END_REC_LEN
	call	WriteBytes
	
	jp	CLOSE2		; close REL file
;
;	Write PSECT records
;
WritePSECTS:
	ld	a,(PBUF)	;buffer loaded?
	or	a
	jr	z,p1
	call	WriteBuf	;yes, then write the buffer
	call	DeployREL	;then write RELOC recs (if any...)
p1:
	ld	a,(RPBUF)	;buffer loaded?
	or	a
	call	nz,WriteRBuf	;yes, then write the buffer

	ld	a,(ASEG_F)	;if ASEG is defined, 
	or	a
	jr	z,pp1
	ld	(CSEG_F),a	;then define also CSEG !
pp1:
	ld	hl,ASEG_REC	;ASEG is default
	ld	b,ASEG_REC_LEN
	call	WriteBytes

	ld	a,(BSEG_F)
	or	a		;if BSS defined
	jr	z,p2
				;write-it
	ld	hl,BSS_REC	;BSS
	ld	b,BSS_REC_LEN
	call	WriteBytes
				;and also BSS size
	ld	hl,(BSSIZE)
	ld	(BSZ),hl

	ld	hl,BSS_S_REC	;BSS size
	ld	b,BSS_S_REC_LEN
	call	WriteBytes
p2:
	ld	a,(DSEG_F)
	or	a		;if DSEG defined
	jr	z,p3
				;write-it
	ld	hl,DSEG_REC	;CSEG
	ld	b,DSEG_REC_LEN
	call	WriteBytes
p3:
	ld	a,(CSEG_F)
	or	a		;if CSEG defined
	jr	z,p4
				;write-it
	ld	hl,CSEG_REC	;CSEG
	ld	b,CSEG_REC_LEN
	call	WriteBytes
p4:
	ld	a,1
	ld	(PSECT_DONE),a	;mark PSECT written...
	ret
;
;	StoreREL
;	E = seg type
;	(PC) = offset
;	BC = symbol vector pointer (in case E=10H)
;
;	RELOC record:
;		1 byte seg type (FF=EOL)
;		1 word offset
;		1 work symbol vector pointer, in case of external ref, else garbage

StoreREL:
	ld	hl,RELBUF	;search EOF mark
sr1:
	ld	a,(hl)
	cp	0FFH
	jr	z,sr2
	inc	hl		;HL=HL+5
	inc	hl
	inc	hl
	inc	hl
	inc	hl
	jr	sr1
sr2:
	ld	(hl),e		;store seg type
	inc	hl
	ld	de,(PC)
	ld	(hl),e		;store PC
	inc	hl
	ld	(hl),d
	inc	hl
	ld	(hl),c		;store symbol vector pointer
	inc	hl
	ld	(hl),b
	inc	hl
	ld	(hl),0FFH	;store EOF mark
	ret
;
;	DeployREL - fetch all REL records and write them
;
;	RELOC record:
;		1 byte seg type (FF=EOL)
;		1 word offset
;		1 work symbol vector pointer, in case of external ref, else garbage
DeployREL:
	ld	hl,RELBUF
loopd:
	ld	a,(hl)
	cp	0FFH		;EOL?
	jr	nz,d1
	ld	hl,RELBUF	;yes, reset buffer
	ld	(hl),0FFH
	call	WriteRBuf	;write RELOC records
	ret			;and return
d1:	
	ld	c,a		;C=seg type
	inc	hl
	ld	e,(hl)
	inc	hl
	ld	d,(hl)		;DE = offset
	inc	hl
	push	hl		;save pointer in REL buffer
	push	bc		;save seg type
				;adjust offset
	ex	de,hl		;HL = offset
	ld	bc,(LASTBASE)
	or	a		;CARRY=0
	sbc	hl,bc
	ex	de,hl		;DE = adjusted offset (offset-LASTBASE)

	ld	a,(RPBUF)	;if buffer empty
	or	a
	call	z,SetRelocType	;prepare a RELOC record
	ld	a,(RPBUF)
	cp	RSAFE		;if safe limit reached
	jr	c,d0
	call	WriteRBuf	;first write buffer
	call	SetRelocType	;and setup RELOC buffer
d0:
	ld	b,d
	ld	c,e		;BC=offset
	call	StoreRWord

	pop	bc		;C=seg type
	ld	a,c		;what kind of seg?
	cp	10H		;external reference?
	jr	nz,d2
				;yes
	ld	a,22H		;"by value of"
	call	StoreRByte
				;store the external symbol
	pop	hl		;restore pointer in REL buffer
	push	hl		;and save-it back
	ld	e,(hl)
	inc	hl
	ld	d,(hl)		;DE=pointer of symbol vector
	ld	a,(de)		;A=mode & len
	and	1FH		;keep 5 bits, A=length
	inc	de		;DE=symbol pointer
	ld	b,a
loops:	ld	a,(de)
	inc	de
	call	StoreRByte	
	djnz	loops
	xor	a		;...and the final 0
	call	StoreRByte
	jr	d6		;then continue fetching
d2:				;not external ref
	ld	a,12H		;"within psect"
	call	StoreRByte
	ld	a,c
	cp	40H		;TEXT?
	jr	nz,d3
	ld	hl,TEXT_T
	ld	bc,4
	jr	d5
d3:	cp	80H		;DATA?
	jr	nz,d4
	ld	hl,DATA_T
	ld	bc,4
	jr	d5
d4:	ld	hl,BSS_T	;it's BSS
	ld	bc,3
d5:	
	ld	de,(RPBUF)
	ldir			;copy seg name to buffer
	xor	a
	ld	(de),a		;and also the 0
	inc	e
	ld	(RPBUF),de
d6:
	pop	hl		;HL=pointer in REL buffer
	inc	hl		;skip pointer of symbol vector
	inc	hl
	jp	loopd
;	
; BUFFER related ----------------------------------------------------
;
;	Store SYM records prefix to buffer
;	and init buffer params
;	A,HL affected
;
SetSymType:
	ld	hl,(BUFFER)	;skip records group length
	inc	l
	inc	l
	ld	(hl),R_SYM	;store records group type
	inc	l
	ld	(PBUF),hl	;set pointer
	ret
;
;	Store TEXT records prefix to buffer
;	and init buffer params
;	A,BC,DE,HL affected
;
SetTextType:
	ld	hl,(BUFFER)	;skip records group length
	inc	l
	inc	l
	ld	(hl),R_TEXT	;store records group type
	inc	l
	ld	bc,(PC)
	ld	(hl),c		;store PC
	inc	l
	ld	(hl),b
	inc	l
	ld	(hl),0		;plus a null word
	inc	l
	ld	(hl),0
	inc	l
	ex	de,hl
				;store seg name
	ld	a,(CRTGRP)	;80H:data, 40H:text, 00H:''
	or	a
	ld	hl,TEXT_T+4	;pointer of ''
	ld	bc,1
	jr	z,s1
	ld	c,5		;4+1
	cp	80H		;DATA?
	ld	hl,TEXT_T	;if no, write 'text'
	jr	nz,s1
	ld	hl,DATA_T	;if yes, write 'data'
s1:	
	ldir
	ld	(PBUF),de	;set pointer
	ret
;
;	Write BUFFER to disk
;	A,B,HL affected
;
WriteBuf:
	ld	a,(PBUF)	;empty?
	or	a
	ret	z		;yes, return
	ld	b,a		;counter of bytes to write			
	ld	hl,(BUFFER)
	sub	3
	ld	(hl),a		;set record length field
	xor	a
	ld	(PBUF),a	;reset pointer
	inc	l
	ld	(hl),a		;record length high = 0
	inc	l
	ld	a,(hl)
	cp	R_TEXT		;if TEXT
	jr	nz,wb1
	inc	l
	ld	a,(hl)		;save LASTBASE
	ld	(LASTBASE),a
	inc	l
	ld	a,(hl)
	ld	(LASTBASE+1),a
wb1:
	ld	hl,(BUFFER)
;
;	HL=(BUFFER),B=counter
;
WriteBytes:
	ld	a,(hl)
	inc	hl
	push	hl
	push	bc
	call	WNB2
	pop	bc
	pop	hl
	djnz	WriteBytes
	ret
;
;	Store byte in BUFFER
;	A = byte
;	HL affected
;
StoreByte:
	ld	hl,(PBUF)
	ld	(hl),a
	inc	l
	ld	(PBUF),hl
	ret
;
;	Store word in BUFFER
;	BC=word
;	HL affected
;
StoreWord:
	ld	hl,(PBUF)
	ld	(hl),c
	inc	l
	ld	(hl),b
	inc	l
	ld	(PBUF),hl
	ret
;
;	Write Symbol to buffer
;	DE = value
;	HL = name, B = length
;	C = seg type & mode
;
WriteSym:
	push	hl		;save name pointer
	ld	hl,(PBUF)
	ld	(hl),e		;value
	inc	l
	ld	(hl),d
	inc	l
	xor	a
	ld	(hl),a
	inc	l
	ld	(hl),a
	inc	l
				;type mark
	ld	a,c
	and	EXTSYM
	jr	z,ws1
				;external 1600H
	ld	a,16H
	jr	ws3	
	
ws1:	ld	a,c
	and	GBLSYM
	jr	z,ws2
				;global(public) 1000H
	ld	a,10H
	jr	ws3
ws2:				;local 0000H
	xor	a
ws3:
	ld	(hl),a
	inc	l
	xor	a
	ld	(hl),a
	inc	l
				;psect name
	ld	a,c
	and	EXTSYM		;EXTERN?
	jr	z,ws4
				;yes, extern psect = ''
	ld	(hl),0		;store only a zero
	inc	l
	jr	ws8		;write symbol name
ws4:
	ld	a,c
	and	0C0H		;keep seg type
	cp	80H		;DSEG?
	jr	nz,ws5
				;data
	ld	de,DATA_T
	ld	c,5
	jr	ws7
ws5:
	cp	0C0H		;BSS?
	jr	nz,ws6
				;bss
	ld	de,BSS_T
	ld	c,4
	jr	ws7
ws6:				;else TEXT
	ld	de,TEXT_T
	ld	c,5
ws7:
				;store psect name+zero
loopp:	ld	a,(de)
	inc	de
	ld	(hl),a
	inc	l
	dec	c
	jr	nz,loopp
ws8:
	pop	de		;DE=name, B=name length
				;store symbol name + zero
loopn:	ld	a,(de)
	inc	de
	ld	(hl),a
	inc	l
	dec	b
	jr	nz,loopn

	ld	(hl),0
	inc	l

	ld	(PBUF),hl
	ret
;
; RBUFFER related ----------------------------------------------------
;
;	Write RBUFFER to disk
;	A,B,HL affected
;
WriteRBuf:
	ld	a,(RPBUF)	;empty?
	or	a
	ret	z		;yes, return
	ld	b,a		;counter of bytes to write
	ld	hl,(RBUFFER)		
	sub	3		
	ld	(hl),a		;set record length field
	xor	a
	inc	l
	ld	(hl),a
	ld	(RPBUF),a	;reset pointer
	dec	l		
;
;	HL=(RBUFFER),B=counter
;
WriteRBytes:
	ld	a,(hl)
	inc	l
	push	hl
	push	bc
	push	de
	call	WNB2
	pop	de
	pop	bc
	pop	hl
	djnz	WriteRBytes
	ret
;
;	Store byte in RBUFFER
;	A = byte
;	HL affected
;
StoreRByte:
	ld	hl,(RPBUF)
	ld	(hl),a
	inc	l
	ld	(RPBUF),hl
	ret
;
;	Store word in RBUFFER
;	BC=word
;	HL affected
;
StoreRWord:
	ld	hl,(RPBUF)
	ld	(hl),c
	inc	l
	ld	(hl),b
	inc	l
	ld	(RPBUF),hl
	ret
;
;	Store RELOC prefix to buffer
;	A,HL affected
;
SetRelocType:
	ld	hl,(RBUFFER)	;skip records group length
	inc	l
	inc	l
	ld	(hl),R_RELOC	;store records group type
	inc	l
	ld	(RPBUF),hl	;set pointer
	ret
;
	psect	data
;
;	default records
;
ID_REC:
	defw	0AH		;len
	defb	R_IDENT		;IDENT
	defb	0,1,2,3
	defb	0,1
	defm	'Z80'
	defb	0
ID_REC_LEN	equ	$-ID_REC

START_REC:
	defw	9		;4+4+1
	defb	R_START
start:	defs	2
	defw	0
pstart:	defm	'text'
	defb	0
START_REC_LEN	equ	$-START_REC

END_REC:
	defw	2		;len
	defb	R_END		;END
	defw	0
END_REC_LEN	equ	$-END_REC

ASEG_REC:			;psect text,abs
	defw	3		;len
	defb	R_PSECT		;PSECT
	defb	0D0H,00H	;seg mark
	defb	0		;seg name = ''
ASEG_REC_LEN	equ	$-ASEG_REC

BSS_REC:			;psect bss
	defw	6		;len
	defb	R_PSECT		;PSECT
	defb	10H,00H		;seg mark
	defm	'bss'		;seg name
	defb	0
BSS_REC_LEN	equ	$-BSS_REC

BSS_S_REC:
	defw	8		;len
	defb	R_TEXT		;TEXT
BSZ:	defs	2		;STORE HERE BSSIZE !
	defw	0
	defm	'bss'		;seg name
	defb	0
BSS_S_REC_LEN	equ	$-BSS_S_REC

DSEG_REC:			;psect data
	defw	7		;len
	defb	R_PSECT		;PSECT
	defb	10H,00H		;seg mark
	defm	'data'		;seg name
	defb	0
DSEG_REC_LEN	equ	$-DSEG_REC

CSEG_REC:			;psect text
	defw	7		;len
	defb	R_PSECT		;PSECT
	defb	10H,00H		;seg mark
	defm	'text'		;seg name
	defb	0
CSEG_REC_LEN	equ	$-CSEG_REC

BIGBUF:	defs	300H		;here will be placed the two buffers
				;aligned at xx00H
BUFFER:	defs	2		;to hold groups of records (TEXT or SYM)
RBUFFER:defs	2		;to hold groups of records (RELOC)
;
PBUF:	defs	2		;pointer in BUFFER
;
RPBUF:	defs	2		;pointer in RBUFFER
;
SAFE	equ	0FEH		;max number of bytes in TEXT buffer
RSAFE	equ	0D0H		;max number of bytes in RELOC & SYM buffer
				; largest rec length is 32 + 5 + 2 + 4
;
;	records group type
;
R_NONE	equ	0
R_TEXT	equ	1
R_PSECT equ	2
R_RELOC	equ	3
R_SYM	equ	4
R_START	equ	5
R_END	equ	6
R_IDENT equ	7
;
DATA_T:	defm	'data'
	defb	0
TEXT_T:	defm	'text'
	defb	0
BSS_T:	defm	'bss'
	defb	0
;
ASEG_F:	defs	1		;1 if present,else 0
CSEG_F:	defs	1		;1 if present,else 0
DSEG_F:	defs	1		;1 if present,else 0
BSEG_F:	defs	1		;1 if present,else 0
;
PSECT_DONE:defs	1		;1 if PSECT records were written
;
CRTGRP:defs	1		;current records group type
;
PC:	defs	2		;current segment PC
;
RELBUF:	defs	425		;reloc records buffer (85 x 5)
;
;	RELOC record:
;		1 byte seg type (FF=EOL)
;		1 word offset
;		1 work symbol vector pointer, in case of external ref, else garbage
;
LASTBASE:defs	2		;last start PC for a TEXT record
LASTEXTSYM:defs	2		;last external symbol (pointer in symbols table)
;
;order of groups
;
;IDENT
;TEXT & RELOC
;PSECT (ASEG mark,BSS,BSS size,DATA,TEXT)
;SYM
;END
;
