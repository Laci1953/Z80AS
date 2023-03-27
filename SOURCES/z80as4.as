*include	zsm.inc

	global	GNC
	global	CL1
	global	BACKUP
	global	EVALREG
	global	OERROR
	global	CPU
	global	CMPHD
	global	OPCODE
	global	EMITB
	global	EVALNEW
	global	VAL
	global	EVMODE
	global	REQ8U
	global	SWITCH
	global	S360
	global	S370
	global	S390
	global	S391
	global	S380
	global	S392
	global	S500
	global	S520
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
	global	UCASE
	global	PTR1
	global	JOPTDIS
	global	IDADR
	global	NEWSYM
	global	SYMADR
	global	JFLAG
	global	PASSNO
	global	MERROR
	global	SETMDF
	global	UFLAG
	global	EQUFLG
	global	ISDIG
	global	atof
	global	fperr
	global	ERRFLG
	global	AM9511F
	global	DSFLAG
	global	LENDS
	global	RELERR
	global	DBWFLG
	global	CCONST
	global	EMITV
	global	ENDMARK
	global	ENDADR
	global	ENDMOD
	global	EFLG
	global	CLEVEL
	global	MDFLVL
	global	SYMPTR
	global	PC
	global	SAVEPC
	global	CURSEG
	global	WLOC
	global	LOCFLG
	global	VALERR
	global	MAXLNE
	global	CURLNE
	global	NOLIST
	global	CONDSP
	global	IFLIST
	global	ID
	global	SYMTBL
	global	SYMLUK
	global	MCHECK
	global	WERROR
	global	GETSTR
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
;       SUBTTL  Instruction Class 16 - INCW, DECW
;
;       Class 16 - INCW, DECW (Z280)
;
CL16:   
;	call    GNC
;        or      a
;        jp      z,OERROR
;        cp      '('
;        jp      z,S471          ; branch if () form
;        cp      '<'
;        jp      z,S475          ; branch if <> form
;        call    BACKUP
;        call    EVALREG
;        cp      RPNAME
;        jp      nz,OERROR       ; must be rpair
;        jp      S270            ; handle like INC, DEC
;
;S471:   call    EVALSRG
;        cp      RSNAME
;        jr      z,S476          ; branch if special reg
;        cp      RNAME
;        jp      z,OERROR
;        cp      RPNAME
;        jr      nz,S472
;        ld      a,l
;        cp      4
;        jp      nz,OERROR       ; only HL,IX,IY allowed
;        ld      a,h
;        or      a
;        jr      z,S473          ; branch if HL
;        ld      c,a
;        ld      a,0FDh
;        call    EMITB
;        ld      a,c
;        rrca
;        and     10h             ; DD becomes 00h, FD becomes 10h
;        ld      c,a
;        ld      a,(OPCODE+1)
;        add     a,c
;        call    EMITB
;        call    EVALNEW
;        call    EMITV           ; emit address
;        call    REQCHR
;        defb    ')'
;        ret
;
;       INCW    (addr)
;
;S472:
;	ld      a,0DDh
;        call    EMITB
;        ld      a,(OPCODE+1)
;        add     a,10h
;        call    EMITB
;        call    EMITV           ; emit address
;        call    REQCHR
;        defb    ')'
;        ret
;
;       INCW    (HL)
;
;S473:   ld      a,0DDh
;        call    EMITB
;        ld      a,(OPCODE+1)
;        call    EMITB
;        call    REQCHR
;        defb    ')'
;        ret
;
;       INCW    <addr>
;
;S475:   call    EVALNEW
;        ld      a,0DDh
;        call    EMITB
;        ld      a,(OPCODE+1)
;        add     a,30h
;        call    EMITB
;        ld      de,0
;        call    EMITVR          ; emit relative address
;        call    REQCHR
;        defb    '>'
;        ret
;
;       INCW    (PC+addr)
;
;S476:   ld      a,l
;        cp      3
;        jp      nz,OERROR       ; only PC allowed
;        call    EVALNEW         ; get index
;        ld      a,0DDh
;        call    EMITB
;        ld      a,(OPCODE+1)
;        add     a,30h
;        call    EMITB
;        call    EMITV           ; emit address
;        call    REQCHR
;        defb    ')'
;        ret
;
;       SUBTTL  Instruction Class 17 - DIV, DIVU
;
;       Class 17 - DIV, DIVU (Z280)
;
CL17:   
;	call    GNC
;        or      a
;        jp      z,OERROR
;        cp      '('
;        jp      z,S1003         ; branch if () form
;        cp      '<'
;        jp      z,S1003         ; branch if <> form
;        call    BACKUP
;        call    EVALREG
;        cp      RNAME
;        jr      z,S1002         ; branch if single register
;        cp      RPNAME
;        jp      nz,S1001        ; branch if not register
;        ld      de,4
;        call    CMPHD           ; if rpair, only HL is legal
;        jp      nz,OERROR
;        call    GNC
;        cp      ','
;        jp      nz,OERROR       ; and must be the form HL,...
;        call    GNC
;        or      a
;        jp      z,OERROR
;        cp      '('
;        jr      z,S1003         ; branch if () form
;        cp      '<'
;        jp      z,S1003         ; branch if <> form
;        call    BACKUP
;        call    EVALREG
;        cp      RNAME
;        jr      z,S1002         ; branch if single register
;        cp      RPNAME
;        jp      z,OERROR
;S1001:  ld      a,0FDh
;        call    EMITB
;        ld      a,0EDh
;        call    EMITB
;        ld      a,(OPCODE)
;        add     a,38h
;        call    EMITB
;        ld      hl,(VAL)
;        ld      a,(EVMODE)
;        call    REQ8U           ; ensure 8-bit value
;        ld      a,l
;        jp      EMITB
;
;S1002:  ld      a,(VAL+1)
;        or      a
;        call    nz,EMITB
;        ld      a,0EDh
;        call    EMITB
;        ld      a,(VAL)
;        rlca
;        rlca
;        rlca
;        ld      c,a
;        ld      a,(OPCODE)
;        add     a,c
;        jp      EMITB
;
;       DIV     (...)
;
;S1003:  call    EVBRKT          ; evaluate ()
;        ld      hl,DVTBL
;        jp      SWITCH
;
;DVTBL:  defw    S1012           ; (addr)
;        defw    OERROR          ; (r)
;        defw    S1004           ; (rp)
;        defw    S1016           ; (rp+d8)
;        defw    S1018           ; (rp+d16)
;        defw    S1014           ; (x+y)
;        defw    S1010           ; (PC+d16)
;        defw    S1010           ; <addr>
;
;       DIV     (addr)
;
;S1012:  ld      a,0DDh
;        call    EMITB
;        ld      a,0EDh
;        call    EMITB
;        ld      a,(OPCODE)
;        add     a,038h
;        call    EMITB
;        jp      EMITV           ; emit address
;
;       DIV     (rp)
;
;S1004:  ld      a,(REGVAL)
;        cp      6
;        jr      z,S1008         ; branch if (SP)
;        cp      4
;        jp      nz,OERROR       ; else must be (HL)
;
;       DIV     (HL)
;
;        ld      a,0EDh
;        call    EMITB
;        ld      a,(OPCODE)
;        add     a,30h
;        jp      EMITB
;
;       DIV     (IX/IY+d8)
;
;S1016:  ld      a,(REGVAL+1)
;        call    EMITB           ; emit prefix
;        ld      a,0EDh
;        call    EMITB
;        ld      a,(OPCODE)
;        add     a,30h
;        call    EMITB
;        ld      a,(VAL)
;        jp      EMITB
;
;       DIV     (rp+d16)
;
;S1018:  ld      hl,(REGVAL)
;        ld      a,l
;        cp      6
;        jr      z,S1008         ; branch if (SP)
;        ld      e,0FDh
;        ld      a,h
;        cp      0DDh
;        ld      c,08h           ; IX
;        jr      z,S1005
;        cp      0FDh
;        ld      c,10h           ; IY
;        jr      z,S1005
;        ld      c,18h           ; HL
;S1005:  ld      a,e
;        call    EMITB
;        ld      a,0EDh
;        call    EMITB
;        ld      a,(OPCODE)
;        add     a,c
;        call    EMITB
;        jp      EMITV           ; emit address
;
;       DIV     (SP+d16)
;
;S1008:  ld      c,00h
;        ld      e,0DDh
;        jr      S1005
;
;       DIV     (x+y)
;
;S1014:  ld      a,0DDh
;        call    EMITB
;        ld      a,0EDh
;        call    EMITB
;        ld      a,b
;        rlca
;        rlca
;        rlca
;        ld      c,a
;        ld      a,(OPCODE)
;        add     a,c
;        jp      EMITB
;
;       DIV     <addr>
;       DIV     (PC+addr)
;
;S1010:  ld      a,0FDh
;        call    EMITB
;        ld      a,0EDh
;        call    EMITB
;        ld      a,(OPCODE)
;        call    EMITB
;        ld      a,c
;        cp      6
;        jp      z,EMITV         ; emit address if (PC+addr)
;        ld      de,0
;        jp      EMITVR          ; else emit relative address
;
;       SUBTTL  Instruction Class 18 - DIVW, DIVUW
;
;       Class 18 - DIVW, DIVUW (Z280)
;
CL18:   
;	call    GNC
;        or      a
;        jp      z,OERROR
;        cp      '('
;        jr      z,S1140         ; branch if () form
;        cp      '<'
;        jp      z,S1150         ; branch if <> form
;        call    BACKUP
;        call    EVALSRG         ; include special registers
;        cp      RSNAME
;        jr      nz,S1101        ; jump if not special reg
;        ld      a,l
;        dec     a
;        jp      nz,OERROR       ; else must be DEHL
;        call    REQCHR
;        defb    ','
;        call    GNC
;        or      a
;        jp      z,OERROR
;        cp      '('
;        jr      z,S1140         ; branch if () form
;        cp      '<'
;        jp      z,S1150         ; branch if <> form
;        call    BACKUP
;        call    EVALREG
;S1101:  cp      RNAME
;        jp      z,OERROR        ; can't be single reg
;        cp      RPNAME
;        jr      z,S1102         ; branch if rpair
;
;       DIVW    nnn
;
;        ld      a,0FDh
;        call    EMITB
;        ld      a,0EDh
;        call    EMITB
;        ld      a,(OPCODE)
;        add     a,30h
;        call    EMITB
;        jp      EMITV           ; emit address
;
;       DIVW    rpair
;
;S1102:  ld      hl,(VAL)
;        ld      a,h
;        or      a
;        call    nz,EMITB        ; emit prefix for index register
;        ld      a,0EDh
;        call    EMITB
;        ld      a,l
;        rlca
;        rlca
;        rlca
;        ld      l,a
;        ld      a,(OPCODE)
;        add     a,l
;        jp      EMITB
;
;       DIVW    (...)
;
;S1140:  call    EVALSRG
;        cp      RSNAME
;        jr      z,S1151         ; branch if special reg
;        cp      RNAME
;        jp      z,OERROR        ; can't be single reg
;        cp      RPNAME
;        jr      z,S1142         ; branch if rpair
;        ld      a,0DDh
;        ld      c,10h
;S1141:  call    EMITB
;        ld      a,0EDh
;        call    EMITB
;        ld      a,(OPCODE)
;        add     a,c
;        call    EMITB
;        call    EMITV           ; emit address
;        call    REQCHR
;        defb    ')'
;        ret
;
;       DIVW    (rpair)
;
;S1142:  ld      hl,(VAL)
;        ld      a,l
;        cp      4
;        jp      nz,OERROR       ; must be HL/IX/IY
;        ld      a,h
;        or      a
;        jr      z,S1143         ; branch if HL
;        push    hl
;        call    EVALNEW
;        pop     bc
;        ld      a,b
;        cp      0DDh
;        ld      c,0
;        jr      z,S1142A
;        ld      c,10h
;S1142A: ld      a,0FDh
;        jr      S1141
;
;       DIVW    (HL)
;
;S1143:  ld      a,0DDh
;        call    EMITB
;        ld      a,0EDh
;        call    EMITB
;        ld      a,(OPCODE)
;        call    EMITB
;        call    REQCHR
;        defb    ')'
;        ret
;
;       DIVW    <...>
;
;S1150:  call    EVALNEW
;        ld      a,0DDh
;        call    EMITB
;        ld      a,0EDh
;        call    EMITB
;        ld      a,(OPCODE)
;        add     a,30h
;        call    EMITB
;        ld      de,0
;        call    EMITVR          ; emit relative address
;        call    REQCHR
;        defb    '>'
;        ret
;
;       DIVW    (PC+addr)
;
;S1151:  ld      a,l
;        cp      3
;        jp      nz,OERROR       ; only PC allowed
;        call    EVALNEW
;        ld      a,0DDh
;        call    EMITB
;        ld      a,0EDh
;        call    EMITB
;        ld      a,(OPCODE)
;        add     a,30h
;        call    EMITB
;        call    EMITV           ; emit address
;        call    REQCHR
;        defb    ')'
;        ret
;
;       SUBTTL  Instruction Class 19 - LDA (Z280)
;
;       Class 19 - LDA (Z280)
;
CL19:   
;	call    EVALREG
;        cp      RPNAME
;        jp      nz,OERROR
;        ld      a,l
;        cp      4
;        jp      nz,OERROR       ; must be HL/IX/IY
;        ld      a,h
;        or      a
;        call    nz,EMITB        ; emit prefix if index register
;        call    REQCHR
;        defb    ','
;        call    GNC
;        cp      '<'
;        jr      z,S1203         ; branch if <> form
;        cp      '('
;        jp      nz,OERROR       ; else must be () form
;S1203:  call    EVBRKT          ; evaluate ()
;        ld      hl,LDATBL
;        jp      SWITCH
;
;LDATBL: defw    S1201           ; (addr)
;        defw    OERROR          ; (r)
;        defw    S1212           ; (rp)
;        defw    S1212           ; (rp+d8)
;        defw    S1212           ; (rp+d16)
;        defw    S1210           ; (x+y)
;        defw    S1200           ; (PC+d16)
;        defw    S1200           ; <addr>
;
;       LDA     rp,(addr)
;
;S1201:  ld      a,21h
;        call    EMITB
;        jp      EMITV
;
;       LDA     rp,(rp[+d16])
;
;S1212:  ld      hl,(REGVAL)
;        ld      a,l
;        cp      6
;        ld      c,02h
;        jr      z,S1204         ; branch if SP
;        cp      4
;        jp      nz,OERROR       ; else must be HL/IX/IY
;        ld      a,h
;        cp      0DDh            ; IX
;        ld      c,2Ah
;        jr      z,S1204
;        cp      0FDh            ; IY
;        ld      c,32h
;        jr      z,S1204
;        ld      c,3Ah           ; else is HL
;S1204:  ld      a,0EDh
;        call    EMITB
;        ld      a,c
;        call    EMITB
;        jp      EMITV
;
;       LDA     rp,(x+y)
;
;S1210:  ld      a,0EDh
;        call    EMITB
;        ld      a,b
;        rlca
;        rlca
;        rlca
;        add     a,02h
;        jp      EMITB
;
;       LDA     rp,<addr>
;       LDA     rp,(PC+addr)
;
;S1200:  ld      a,0EDh
;        call    EMITB
;        ld      a,22h
;        call    EMITB
;        ld      a,c
;        cp      6
;        jp      z,EMITV         ; emit address if (PC+addr)
;        ld      de,0
;        jp      EMITVR          ; else emit relative address
;
;       SUBTTL  Instruction Class 20 - LDCTL (Z280)
;
;       Class 20 - LDCTL (Z280)
;
CL20:
;	call    GNC
;        or      a
;        jp      z,OERROR
;        cp      '('
;        jp      z,S1301         ; branch if () form
;        call    BACKUP
;        call    EVALSRG         ; include special regs
;        cp      RSNAME
;        jr      z,S1310         ; branch if special register
;        cp      RPNAME
;        jp      nz,OERROR       ; else must be rpair
;        ld      a,l
;        cp      4
;        jp      nz,OERROR       ; only HL/IX/IY allowed
;        ld      a,h
;        or      a
;        call    nz,EMITB        ; emit prefix if index register
;        call    REQCHR
;        defb    ','
;        call    GNC
;        or      a
;        jp      z,OERROR
;        cp      '('
;        jr      z,S1320         ; branch if () form
;        call    BACKUP
;        call    EVALSRG
;        cp      RSNAME
;        jp      nz,OERROR       ; must be special reg
;        ld      a,l
;        cp      2
;        jp      nz,OERROR       ; only USP allowed
;        ld      a,0EDh
;        call    EMITB
;        ld      a,87h
;        jp      EMITB
;
;       LDCTL   rp,(C)
;
;S1320:  call    EVALREG
;        cp      RNAME
;        jp      nz,OERROR
;        ld      a,l
;        dec     a
;        jp      nz,OERROR       ; only C reg allowed
;        call    REQCHR
;        defb    ')'
;        ld      a,0EDh
;        call    EMITB
;        ld      a,66h
;        jp      EMITB
;
;       LDCTL   USP,rp
;
;S1310:  ld      a,(VAL)
;        cp      2
;        jp      nz,OERROR       ; only USP allowed
;        call    REQCHR
;        defb    ','
;        call    EVALREG
;        cp      RPNAME
;        jp      nz,OERROR       ; must be rpair
;        ld      a,l
;        cp      4
;        jp      nz,OERROR       ; only HL/IX/IY allowed
;        ld      a,h
;        or      a
;        call    nz,EMITB        ; emit prefix if index register
;        ld      a,0EDh
;        call    EMITB
;        ld      a,8Fh
;        jp      EMITB
;
;       LDCTL   (C),rp
;
;S1301:  call    EVALREG
;        cp      RNAME
;        jp      nz,OERROR
;        ld      a,l
;        dec     a
;        jp      nz,OERROR       ; only C reg allowed
;        call    REQCHR
;        defb    ')'
;        call    REQCHR
;        defb    ','
;        call    EVALREG         ; get second operand
;        cp      RPNAME
;        jp      nz,OERROR
;        ld      a,l
;        cp      4
;        jp      nz,OERROR       ; only HL/IX/IY allowed
;        ld      a,h
;        or      a
;        call    nz,EMITB        ; emit prefix if index register
;        ld      a,0EDh
;        call    EMITB
;        ld      a,6Eh
;        jp      EMITB
;
;       SUBTTL  Instruction Class 21 - LDUD, LDUP (Z280)
;
;       Class 21 - LDUD, LDUP (Z280)
;
CL21:
;	call    GNC
;        cp      '('
;        jr      z,S1401         ; jump if ()
;        cp      'A'
;        jp      nz,OERROR       ; else first op must be reg A
;        call    REQCHR
;        defb    ','
;        call    REQCHR
;        defb    '('
;        ld      c,0
;S1400:  push    bc
;        call    EVALREG
;        pop     bc
;        cp      RPNAME
;        jp      nz,OERROR       ; second op must be rpair
;        ld      a,l
;        cp      4
;        jp      nz,OERROR       ; only HL/IX/IY allowed
;        ld      a,h
;        or      a
;        call    nz,EMITB        ; emit prefix if index register
;        ld      a,0EDh
;        call    EMITB
;        ld      a,(OPCODE)
;        add     a,c
;        call    EMITB
;        ld      a,h
;        or      a
;        jr      z,S1402         ; finish if HL
;        call    EVALNEW         ; else get displacement
;        ld      a,(EVMODE)
;        call    REQ8U           ; ensure 8-bit value (REQ8S?)
;        ld      a,l
;        call    EMITB
;S1402:  call    REQCHR
;        defb    ')'
;        ret
;
;S1401:  ld      c,08h
;        call    S1400           ; process () via common code
;        call    REQCHR
;        defb    ','
;        call    REQCHR
;        defb    'A'
;        ret
;
;       SUBTTL  Instruction Class 22 - MULT, MULTU (Z280)
;
;       Class 22 - MULT, MULTU (Z280)
;
CL22:
;   	call    GNC
;        or      a
;        jp      z,OERROR
;        cp      '('
;        jp      z,S1505         ; branch if () form
;        cp      '<'
;        jp      z,S1505         ; branch if <> form
;        call    BACKUP
;        call    EVALREG
;        cp      RPNAME
;        jp      z,OERROR        ; rpair illegal here
;        cp      RNAME
;        jp      nz,S1502        ; process immediate value
;
;       MULT A,...
;
;        call    GNC
;        cp      ','             ; check for A,xx form
;        jr      nz,S1504
;        ld      a,(VAL)
;        cp      7
;        jp      nz,OERROR       ; first operand can be only A
;        call    GNC
;        or      a
;        jp      z,OERROR
;        cp      '('
;        jr      z,S1505         ; jump if A,(rp)
;        cp      '<'
;        jp      z,S1505         ; jump if A,<nnnn>
;        call    BACKUP
;        call    EVALREG
;        cp      RPNAME
;        jp      z,OERROR        ; rpair illegal here
;        cp      RNAME
;        jr      nz,S1502        ; process immediate value
;S1504:  ld      a,(VAL+1)
;        or      a               ; check for index register
;        call    nz,EMITB        ; emit prefix
;        ld      a,0EDh
;        call    EMITB
;        ld      a,(VAL)
;        rlca
;        rlca
;        rlca
;        ld      c,a
;        ld      a,(OPCODE)
;        add     a,c
;        jp      EMITB
;
;       Immediate value - MULT nn
;
;S1502:  ld      a,0FDh
;        call    EMITB
;        ld      a,0EDh
;        call    EMITB
;        ld      a,(OPCODE)
;        add     a,38h
;        call    EMITB
;        ld      hl,(VAL)
;        ld      a,(EVMODE)
;        call    REQ8U
;        ld      a,l
;        jp      EMITB
;
;       MULT    (xx)
;
;S1505:  call    EVBRKT          ; evaluate expression in brackets
;        ld      hl,MULTBL
;        jp      SWITCH
;
;MULTBL: defw    S1512           ; (addr)
;        defw    OERROR          ; (r)
;        defw    S1501           ; (rp)
;        defw    S1511           ; (rp+d8)
;        defw    S1503           ; (rp+d16)
;        defw    S1507           ; (x+y)
;        defw    S1516           ; (PC+d16)
;        defw    S1516           ; <addr>
;
;       MULT    (addr)
;
;S1512:  ld      c,38h
;        jr      S1514
;
;       MULT    (rp)
;
;S1501:  ld      a,(REGVAL)
;        cp      6
;        jr      z,S1513         ; branch if (SP)
;        cp      4
;        jp      nz,OERROR       ; else only (HL) allowed
;
;       MULT    (HL)
;
;        ld      a,0EDh
;        call    EMITB
;        ld      a,(OPCODE)
;        add     a,30h
;        jp      EMITB
;
;       MULT    (IX/IY+d8)
;
;S1511:  ld      a,(REGVAL+1)
;        call    EMITB           ; emit prefix for index register
;        ld      a,0EDh
;        call    EMITB
;        ld      a,(OPCODE)
;        add     a,30h
;        call    EMITB
;        ld      a,(VAL)
;        jp      EMITB
;
;       MULT    (rp+d16)
;
;S1503:  ld      hl,(REGVAL)
;        ld      a,l
;        cp      6
;        jr      z,S1513         ; branch if (SP)
;        ld      a,0FDh
;        call    EMITB
;        ld      a,0EDh
;        call    EMITB
;        ld      a,h
;        ld      c,08h           ; IX
;        cp      0DDh
;        jr      z,S1510
;        ld      c,10h           ; IY
;        cp      0FDh
;        jr      z,S1510
;        ld      c,18h
;        jp      S1510
;
;       MULT    (SP+nnnn)
;
;S1513:  ld      c,00h
;S1514:  ld      a,0DDh
;        call    EMITB
;        ld      a,0EDh
;        call    EMITB
;S1510:  ld      a,(OPCODE)
;        add     a,c
;        call    EMITB
;        jp      EMITV           ; emit address
;
;       MULT    (x+y)
;
;S1507:  ld      a,0DDh
;        call    EMITB
;        ld      a,0EDh
;        call    EMITB
;        ld      a,b
;        rlca
;        rlca
;        rlca
;        ld      c,a
;        ld      a,(OPCODE)
;        add     a,c
;        jp      EMITB
;
;       MULT    <nnnn>
;       MULT    (PC+nnnn)
;
;S1516:  ld      a,0FDh
;        call    EMITB
;        ld      a,0EDh
;        call    EMITB
;        ld      a,(OPCODE)
;        call    EMITB
;        ld      a,c
;        cp      6
;        jp      z,EMITV         ; emit address if (PC+addr)
;        ld      de,0
;        jp      EMITVR          ; else emit relative address
;
;       SUBTTL  Instruction Class 23 - CPL, NEG, EXTS
;
;       Class 23 - CPL, NEG, EXTS
;
CL23:   call    GNC
        or      a               ; any operand?
        jp      z,CL1           ; handle like Class 1 (simple opcode)
                                ;  if not (assume reg A)
        call    BACKUP
        call    EVALREG         ; get operand
        cp      RNAME
        jp      z,S1601         ; branch if single reg
        cp      RPNAME
        jp      nz,OERROR       ; else must be rpair
        ld      a,(CPU)
        cp      2
        jp      nz,OERROR       ; Z280 only
        ld      de,4
        call    CMPHD
        jp      nz,OERROR       ; only HL is valid here
        ld      de,(OPCODE)
        ld      a,e
        cp      2Fh
        jp      z,OERROR        ; instruction can't be CPL
        call    EMITB
        ld      a,d
        add     a,08h
        jp      EMITB
;
S1601:  ld      a,l
        cp      7
        jp      nz,OERROR       ; only A is valid here
        jp      CL1             ; emit opcode via Class 1 code

;       SUBTTL  Instruction Class 24 - DI, EI
;
;       Class 24 - DI, EI
;
CL24:   ld      a,(CPU)
        cp      2               ; Z280 mode?
        jp      nz,CL1          ; emit opcode via Class 1 code if not
        call    GNC
        or      a               ; any operand?
        jp      z,CL1           ; emit opcode via Class 1 code if not
        call    BACKUP
        call    EVALNEW         ; get argument
        ld      a,0EDh
        call    EMITB
        ld      a,(OPCODE)
        cp      0F3h            ; DI?
        ld      a,77h
        jr      z,S1701
        ld      a,7Fh
S1701:  call    EMITB
        ld      hl,(VAL)
        ld      a,(EVMODE)
        call    REQ8U
        ld      a,l
        jp      EMITB

;       SUBTTL  Instruction Class 25 - SC (Z280)
;
;       Class 25 - SC (Z280)
;
CL25:
;	call    EVALNEW
;        ld      hl,(OPCODE)
;        call    EMITW
;        jp      EMITV
;
;       SUBTTL  Instruction Class 26 - EPUM, MEPU (Z280)
;
;       Class 26 - EPUM, MEPU (Z280)
;
CL26:   
;	call    GNC
;        cp      '('
;        jr      z,S1801
;        cp      '<'
;        jp      nz,OERROR
;S1801:  call    EVBRKT          ; evaluate ()
;        ld      hl,EPTBL
;        jp      SWITCH
;
;EPTBL:  defw    S1810           ; (addr)
;        defw    OERROR          ; (r)
;        defw    S1815           ; (rp)
;        defw    S1820           ; (rp+d8)
;        defw    S1820           ; (rp+d16)
;        defw    S1830           ; (x+y)
;        defw    S1840           ; (PC+d16)
;        defw    S1840           ; <addr>
;
;       EPUM    (addr)
;
;S1810:  ld      hl,(OPCODE)
;        ld      a,h
;        cp      84h
;        ld      h,0A7h
;        jr      z,S1811         ; the Z280 is inconsistent here
;        ld      h,0AFh
;S1811:  call    EMITW
;        jp      EMITV
;
;       EPUM    (rp)
;
;S1815:  ld      a,(REGVAL)
;        cp      6
;        jr      z,S1820         ; branch if (SP)
;        cp      4
;        jp      nz,OERROR       ; else must be (HL)
;
;       EPUM    (HL)
;
;        ld      hl,(OPCODE)
;        ld      a,l
;        call    EMITB
;        ld      a,h
;        cp      84h
;        ld      a,0A6h
;        jp      z,EMITB
;        ld      a,0AEh
;        jp      EMITB
;
;       EPUM    (rp+d16)
;
;S1820:  ld      hl,(REGVAL)
;        ld      a,l
;        cp      6
;        ld      c,00h
;        jr      z,S1821         ; branch if SP
;        cp      4
;        jp      nz,OERROR       ; else must be HL/IX/IY
;        ld      a,h
;        cp      0DDh
;        ld      c,28h           ; IX
;        jr      z,S1821
;        cp      0FDh
;        ld      c,30h           ; IY
;        jr      z,S1821
;        ld      c,38h           ; HL
;S1821:  ld      hl,(OPCODE)
;        ld      a,l
;        call    EMITB
;        ld      a,h
;        add     a,c
;        call    EMITB
;        jp      EMITV
;
;       EPUM    (x+y)
;
;S1830:  ld      hl,(OPCODE)
;        ld      a,l
;        call    EMITB
;        ld      a,b
;        rlca
;        rlca
;        rlca
;        add     a,h
;        jp      EMITB
;
;       EPUM    <addr>
;       EPUM    (PC+addr)
;
;S1840:  ld      hl,(OPCODE)
;        ld      a,l
;        call    EMITB
;        ld      a,h
;        add     a,20h
;        call    EMITB
;        ld      a,c
;        cp      6
;        jp      z,EMITV         ; emit address if (PC+addr)
;        ld      de,0
;        jp      EMITVR          ; else emit relative address
;
;       SUBTTL  Instruction Class 27 - Pseudo-operators
;
;       Class 27 - Pseudo operators
;
CL27:   ld      a,(OPCODE)
        dec     a
        ld      hl,PSDTAB
        jp      SWITCH

	psect	data

;
PSDTAB: defw    S300            ; EQU,ASET,DEFL
        defw    S307            ; DEFS,DS
        defw    S308            ; DEFB,DEFC,DEFM,DEFZ,DB,DC
        defw    S317            ; DEFW,DW
        defw    S320            ; END
        defw    S323            ; ORG
        defw    S324            ; FORM,PAGE,EJECT
        defw    S330            ; IF
        defw    S340            ; ELSE
        defw    S350            ; ENDIF
        defw    S360            ; LIST,.LIST,.XLIST,.LALL,.XALL,.SALL,etc.
        defw    S370            ; TITLE,SUBTTL
        defw    S390            ; CSEG
        defw    S391            ; DSEG
        defw    S380            ; ASEG
        defw    S392            ; COMMON
        defw    S500            ; PUBLIC
        defw    S520            ; EXTERN
        defw    S530            ; .Z80,.Z180,.Z280
        defw    S540            ; .EVEN,.ODD
        defw    S550            ; NAME
        defw    S560            ; IDENT
        defw    S570            ; INCLUDE,MACLIB
        defw    S580            ; MACRO,RETP,IRP,IRPC
        defw    S590            ; ENDM
        defw    S600            ; EXITM
        defw    S610            ; LOCAL
        defw    S620            ; RQST,.REQUEST
        defw    S630            ; .PHASE,.DEPHASE
        defw    S640            ; .RADIX
        defw    S650            ; .PRINTX
        defw    S660            ; .COMMENT
        defw    S670            ; PSECT
	defw	SDEFF		; DEFF
	defw	SJOPT		; JOPT
	defw	SetAM9511	; *AM9511

	psect	text

;
;	JOPT ON / OFF
;
SJOPT:
	call	GNC		;if line ends here
	or	a
	jp	z,OERROR	;signal error & return
	call	UCASE
	cp	'O'
	jp	nz,OERROR
	ld	hl,(PTR1)		; pointer to next char
	ld	a,(hl)
	call	UCASE
	cp	'N'
	jp	nz,off?
	ld	a,0FFH
setjopt:ld	(JOPTDIS),a	;enable
	inc	hl
	ld	(PTR1),hl
	ret
off?:	cp	'F'
	jp	nz,OERROR
	inc	hl
	ld	a,(hl)
	call	UCASE
	cp	'F'
	jp	nz,OERROR
	xor	a		;disable
	jr	setjopt
;
;       EQU, ASET, DEFL
;
S300:   ld      hl,(IDADR)      ; check for preceding symbol
        ld      a,h
        or      l
        jp      z,OERROR        ; error if not present
        ld      c,(hl)          ; save identity char
        push    bc
        call    EVALNEW         ; get value
        pop     bc

        ld      a,(OPCODE+1)
        or      a               ; ASET/DEFL?
        ld      b,DFLNAME
        jr      nz,S301         ; branch if yes
        ld      b,EQUNAME       ; else type is EQU

S301:   ld      a,(NEWSYM)
        or      a               ; new symbol?
        jr      nz,S305         ; branch if yes

        ld      a,c             ; fetch identity char
        and     0E0h
        cp      MULTDEF         ; multiple defined?
        jr      z,S304          ; multi defined error if yes

        ld      hl,(SYMADR)
        inc     hl
        inc     hl
        ld      a,(hl)
        and     UNDEF           ; UNDEF bit set?
        jr      nz,S305         ; handle like new symbol if yes

        ld      a,(hl)
        and     EXTSYM          ; External bit set?
        jr      nz,S304         ; multi-defined error if yes

        ld      a,c
        and     0E0h
        cp      b               ; same type? (EQU or DEFL)
        jr      nz,S304         ; multi defined error if not
        cp      DFLNAME         ; DEFL?
        jr      z,S305          ; set/modify value if yes

        call    CMPSYM          ; same value and mode?
        jr      nc,S306         ; return if yes, else is multi def error

S304:   ld	a,(JFLAG)	;if J option ON
	or	a
	jr	z,S304A
	ld	bc,(VAL)	;just store the value
	ld	hl,(SYMADR)
	ld	(hl),c
	inc	hl
	ld	(hl),b
	jr	S306
S304A:
	ld      a,(PASSNO)
        or      a
        jr      z,S303          ; branch if pass 1
        call    MERROR          ; else output multi defined error
        jr      S306
S303:   call    SETMDF          ; set multi defined flag
        jr      S306
;
;       New symbol
;
S305:   ld      a,(OPCODE+1)
        or      a               ; ASET/DEFL?
        ld      b,DFLNAME
        jr      nz,S305A        ; branch if yes
        ld      b,EQUNAME       ; else type is EQU
S305A:  ld      hl,(SYMADR)
        ld      de,(VAL)
        ld      (hl),e          ; store value
        inc     hl
        ld      (hl),d
        inc     hl
        ld      a,(EVMODE)
        ld      (hl),a          ; set mode

        ld      hl,(IDADR)
        ld      a,(hl)
        and     1Fh             ;0Fh
        or      b               ; set type to EQUNAME or DFLNAME
        ld      (hl),a

        ld      c,a
        ld      a,(UFLAG)
        or      a
        ld      a,c
        jr      z,S306          ; branch if UFLAG not set

        ld      hl,(SYMADR)
        inc     hl
        inc     hl
        ld      a,(hl)
        or      UNDEF           ; else set UNDEF bit (if UFLAG is set
        ld      (hl),a          ;  on pass 2, 'U' error will be automatically
                                ;   generated by LSTOUT.)

S306:   ld      a,1
        ld      (EQUFLG),a
        ret
;
;       Check if symbol matches expected value. Used by EQU, DEFL and label
;       definition routines.
;
CMPSYM: ld      hl,(SYMADR)
        ld      e,(hl)
        inc     hl
        ld      d,(hl)
        push    hl
        ld      hl,(VAL)
        call    CMPHD
        pop     hl
        scf
        ret     nz              ; values differ
        inc     hl
        ld      a,(EVMODE)
        xor     (hl)
        and     SEGMASK .or. EXTSYM        ; ignore PUBLIC and UNDEF bits
        scf
        ret     nz              ; mode differs
        or      a               ; CARRY=0
        ret
;
;	DEFF
;
SDEFF:
	call	GNC		;if line ends here
	or	a
	jp	z,OERROR	;signal error & return
	call	BACKUP		;we are after DEFF
loopd:
	call	GNC		;test first char
	cp	'+'		;must be a +
	jr	z,okff
	cp	'-'		;or -
	jr	z,okff
	call	ISDIG		;or a digit
	jp	c,DEBEND	;else error
okff:	call	BACKUP		;we are back to the first char
	call	atof		;(DE,HL) = float
	ld	a,(fperr)
	or	a
	jr	z,no_overflow
	ld	a,'F'		;mark underflow/overflow
	ld	(ERRFLG),a
no_overflow:
				;check AM9511 flag
	ld	a,(AM9511F)
	or	a		;if AM9511 flag was set...
	jr	z,noAM9511
				;convert to AM9511 format
;HI --> AM
;
;E=byte_0 = m_l_l
;D=byte_1 = m_l_h
;L=byte_2 = m_h
;H=byte_3 = s(1bit)e(7bits)
;
;C=reg_s = H=byte_3 & 80H;
;
	ld	a,h
	and	80H
	ld	c,a
;
;B=reg_e = H=byte_3 & 7FH;
;
	ld	b,h
	res	7,b
;
;    /* If bit 23 is zero, must be zero (or not normalized).
;     */
;    if (byte_2 & 0x80) == 0)
;        return FP_OK;
;
	bit	7,l
	jp	z,noAM9511
;
;    /* Unbias the exponent.
;     */
;    A=reg_e -= 65;
;
	ld	a,b
	sub	65
;
;    /* Range check on exponent.
;     */
;    if (A=reg_e < -64)
;        return FP_ERR;
;
;    if (A=reg_e > 63)
;        return FP_ERR;
;
	bit	7,a		;negative?
	jr	z,1f
				;yes
	cp	-64		; A < -64 ?
	jp	c,AM9511_ERR
	jr	2f
1:
				;positive
	cp	64		;A > 63 ?
	jp	nc,AM9511_ERR
2:
;
;    A=reg_e += 1;
;
	inc	a
;
;    /* Exponent to 7 bit (assumes 2's complement machine)
;     */
;    A=reg_e &= 0x7f;
;
	and	7FH
;
;    /* Merge in sign to exponent.
;     */
;H=byte_3 = C=reg_s | A=reg_e;
;
	or	c
	ld	h,a
;
noAM9511:
	push	hl		;emit the 4 bytes
	push	de
	ld	a,e
	call	EMITB
	pop	de
	ld	a,d
	call	EMITB
	pop	hl
	push	hl
	ld	a,l
	call	EMITB
	pop	hl
	ld	a,h
	call	EMITB
	ld	hl,(PTR1)	;see what's next
	ld	a,(hl)
	or	a
	ret	z		;if ';' or EOL,return
	cp	';'
	ret	z
	call	BACKUP		;we are at the separator
	call	GNC		;get-it
	cp	','		;if not ','
	jp	nz,DEBEND	;signal error
	jr	loopd		;else process the next item
;
AM9511_ERR:
	ld	a,'F'		;mark underflow/overflow
	ld	(ERRFLG),a
	jr	noAM9511
;
SetAM9511:
	ld	a,0FFH		;set AM9511 flag
	ld	(AM9511F),a
	ret
;
;       DEFS, DS
;
S307:   ld      a,1
        ld      (DSFLAG),a
	call    EVALNEW
        ld      (LENDS),hl
        ld      a,(EVMODE)
        or      a
        jp      nz,RELERR
        ret
;
;       DEFB, DEFC, DEFM, DEFZ, DB, DC
;
S308:   ld      a,1
        ld      (DBWFLG),a
        ld      a,(OPCODE+1)
        dec     a
        jp      z,S310          ; branch if DC
S308A:  call    GNC             ; get next non-blank char
        or      a
        jp      z,OERROR
        ld      hl,(PTR1)
        ld      c,a             ; save possible quote char in reg C
        cp      27H             ;' quote?
        jr      z,S308Q
        cp      '"'             ; both single and double allowed
        jr      nz,S308C
S308Q:  push    hl
        call    CCONST          ; character constant?
        pop     hl
        jr      c,S314          ; branch if not to process string
        ld      a,d
        or      a
        jr      nz,S314
S308C:  dec     hl              ; point to prev char
        ld      (PTR1),hl
        call    EVALNEW         ; evaluate expression
        ld      a,(EVMODE)
        call    REQ8U           ; require 8-bit value
        ld      a,l
        call    EMITB           ; output value
S308B:  call    GNC             ; fetch next non-blank char from record
        or      a               ; end of record?
        jr      z,S308D         ; done
        cp      ','             ; separator?
        jp      z,S308A         ; process next item if yes
        jp      DEBEND          ; else expression error
S308D:  ld      a,(OPCODE+1)
        or      a               ; test for DEFZ
        ret     z
        xor     a
        jp      EMITB           ; if DEFZ, emit an extra zero byte
;
S313:   call    EMITB           ; output char
        inc     hl
S314:   ld      a,(hl)
        or      a
        jr      z,ERRQ          ; end without another quote?
        cp      c               ; quote?
        jr      nz,S313         ; loop until found
        inc     hl              ; incr record ptr
        ld      a,(hl)
        cp      c
        jr      z,S313          ; double quotes
        ld      (PTR1),hl
        jr      S308B           ; process terminator/separators
;
DEBEND: ld      a,'E'           ; load expression error
        ld      (ERRFLG),a      ; set flag
        ret                     ; done
;
ERRQ:   ld      a,'Q'
        ld      (ERRFLG),a
        ret
;
;       DC, DEFC
;
S310:   call    GNC             ; get next non-blank char
        or      a
        jp      z,OERROR
        ld      hl,(PTR1)
        ld      c,0
        ld      b,a             ; save possible quote char in reg B
        cp      27H             ;' quote?
        jr      z,S312
        cp      '"'             ; both single and double allowed
        jr      z,S312
        jp      OERROR          ; error if no quote
S311:   ld      e,a
        ld      a,c
        ld      c,e
        or      a
        call    nz,EMITB        ; output char
        inc     hl
S312:   ld      a,(hl)
        or      a
        jr      z,ERRQ          ; end without another quote?
        cp      b               ; quote?
        jr      nz,S311         ; loop until found
        inc     hl              ; incr record ptr
        ld      a,(hl)
        cp      b
        jr      z,S311          ; double quotes
        ld      a,c
        or      80h
        cp      80h
        call    nz,EMITB
        ld      (PTR1),hl
        call    GNC             ; fetch next non-blank char from record
        or      a               ; end of record?
        ret     z               ; done
        cp      ','             ; separator?
        jr      z,S310          ; process next item if yes
        jp      DEBEND          ; else expression error
;
;       DW, DEFW
;
S317:   ld      a,1
        ld      (DBWFLG),a
S317A:  call    GNC
        or      a
        jp      z,OERROR
        ld      hl,(PTR1)
        ld      c,a             ; save possible quote in reg C
        ld      b,0             ; char counter
        cp      27H             ;' quote?
        jr      z,S317Q
        cp      '"'             ; both single and double allowed
        jr      nz,S317C
S317Q:  push    hl
        call    CCONST          ; test for character constant
        pop     hl
        jr      c,S318
S317C:  dec     hl              ; point to prev char
        ld      (PTR1),hl       ; restore new ptr
        call    EVALNEW
        call    EMITV           ; output value
S317B:  call    GNC             ; fetch next non-blank char from record
        or      a               ; end of record?
        ret     z               ; done
        cp      ','             ; separator?
        jp      z,S317A         ; process next item if yes
        jp      DEBEND          ; else expression error
;
S318A:  call    EMITB
        inc     hl
        inc     b               ; chars in this string
S318:   ld      a,(hl)
        or      a
        jp      z,ERRQ          ; end without another quote?
        cp      c
        jr      nz,S318A
        inc     hl              ; incr record ptr
        ld      a,(hl)
        cp      c
        jr      z,S318A         ; double quotes
        ld      (PTR1),hl
        ld      a,b             ; get length of string
        rra                     ; is it even?
        jr      nc,S317B        ; finished if so
        xor     a               ; else load upper byte with 0
        call    EMITB           ;  to make a word
        jr      S317B           ; process terminator/separators
;
;       END
;
S320:   call    GNC             ; argument present (program start address)?
        or      a
        ld      hl,0
        ld      a,0
        jr      z,S320A         ; branch if not
        call    BACKUP
        call    EVALNEW         ; else get argument
	ld	a,0FFH
	ld	(ENDMARK),a	;mark END START
        ld      a,(EVMODE)
S320A:  ld      (ENDADR),hl
        ld      (VAL),hl
        ld      (ENDMOD),a
        ld      (EVMODE),a
        ld      a,1
        ld      (EQUFLG),a
S321:   ld      a,1
        ld      (EFLG),a
        ld      a,(CLEVEL)      ; check conditionals stack level
        or      a               ; at zero?
        call    nz,S322         ; error if not
        ld      a,(MDFLVL)      ; check MACRO def level
        or      a               ; at zero?
        ret     z               ; return if yes
        xor     a               ; else an ENDM is missing somewhere
        ld      (MDFLVL),a      ; reset level
        ld      hl,(SYMPTR)
        ld      (hl),a          ; restore end of symbol table marker
S322:   ld      a,'T'+80h       ; and force error output
        ld      (ERRFLG),a
        ret
;
;       ORG	= ASEG + set PC
;
S323:
	ld	hl,(PC)
	ld	a,h
	or	l
	jr	z,S323A
	xor	a		;an ORG, if not the first pseudo-op
	ld	(JFLAG),a	;disables jump optimization
S323A:
        call    SAVEPC          ; save PC for current segment
	xor	a		
        ld      (CURSEG),a      ; set current segment type to Absolute
        call    EVALNEW
        ld      (PC),hl
        ld      a,(PASSNO)
        or      a
        ld      e,0		; crt seg
        call    nz,WLOC
        xor     a
        ld      (LOCFLG),a
        inc     a
        ld      (EQUFLG),a
        ret
;
;       FORM, PAGE, EJECT
;
S324:   ld      a,(OPCODE+1)
        or      a               ; PAGE?
        jr      z,S325          ; branch if not
        call    GNC
        or      a               ; argument follows?
        jr      z,S325          ; branch if not
        call    BACKUP
        call    EVALNEW         ; else evaluate expression
        ld      a,(EVMODE)
        call    REQ8U           ; result must be 8-bit unsigned, non-reloc
        ld      a,h
        or      a
        jr      nz,S325
        ld      a,l
        cp      10              ; in the range 10..255
        call    c,VALERR
        ld      (MAXLNE),a      ; set new page length
S325:   ld      a,(MAXLNE)
        ld      (CURLNE),a      ; cause hof on record after FORM/PAGE/EJECT
        ld      (NOLIST),a      ; suppress listing of FORM, PAGE, EJECT
        ret
;
;       IFxxx
;
S330:   ld      hl,(CONDSP)
        ld      a,(hl)          ; check current state
        rra
        jr      nc,S333         ; if false, simply inc depth level and return
        ld      a,(OPCODE+1)
        ld      hl,IFTBL
        jp      SWITCH

	psect	data

IFTBL:  defw    ZIF             ; IF
        defw    ZIF1            ; IF1
        defw    ZIF2            ; IF2
        defw    ZIF             ; IFT
        defw    ZIFF            ; IFF
        defw    ZIFDEF          ; IFDEF
        defw    ZIFNDF          ; IFNDEF
        defw    ZIFB            ; IFB
        defw    ZIFNB           ; IFNB
        defw    ZIFIDN          ; IFIDN
        defw    ZIFDIF          ; IFDIF
        defw    ZIF80           ; IFZ80
        defw    ZIF180          ; IFZ180
        defw    ZIF280          ; IFZ280

	psect	text
;
;       IF, IFT
;
ZIF:    ld      c,0
        jr      S331
;
;       IFF
;
ZIFF:   ld      c,0FFh
S331:   push    bc
        call    EVALNEW         ; evaluate the expression
        pop     bc
        ld      a,(ERRFLG)
        cp      ' '
        jr      nz,S332         ; on error, force true state
        ld      a,l             ; test for zero
        or      h               ; zero = false, non zero = true
S332:   add     a,0FFh
        sbc     a,a             ; make value 0FFh or 00h
        xor     c               ; complement result if IFF
S333:   ld      hl,(CONDSP)     ; get conditional stack pointer
        and     (hl)            ; include current state
        and     7Fh             ; clear ELSE bit
        ld      c,a             ; and save for later as new state
        ld      a,(CLEVEL)      ; get the stack depth
        cp      CSTKSZ          ; test for maximum level
        jr      nc,S334         ; error if already at max depth
        inc     a               ; inc to next depth
        ld      (CLEVEL),a      ; and store
        dec     hl              ; push stack
        ld      (CONDSP),hl     ; save new SP
        ld      (hl),c          ; put new state in stack
        ld      a,0FFh          ; suppress IF, ELSE, ENDIF
        ld      (IFLIST),a      ;  if "NOCOND"
        ret                     ;   & finish off
;
S334:   ld      a,'A'           ; stack ov'flow
S335:   ld      (ERRFLG),a
        ret                     ; finish off
;
;       IF1
;
ZIF1:   ld      a,(PASSNO)      ; 00h = pass 1, 0FFh = pass 2
        cpl
        jr      S333
;
;       IF2
;
ZIF2:   ld      a,(PASSNO)      ; 00h = pass 1, 0FFh = pass 2
        jr      S333
;
;       IFZ80
;
ZIF80:  ld      c,0
        jr      S341
;
;       IFZ180
;
ZIF180: ld      c,1
        jr      S341
;
;       IFZ280
;
ZIF280: ld      c,2
S341:   ld      a,(CPU)
        sub     c               ; 00h if CPU matches
        ld      c,0FFh
        jr      S332
;
;       IFDEF
;
ZIFDEF: ld      c,0
        jr      S336
;
;       IFNDEF
;
ZIFNDF: ld      c,0FFh
S336:   push    bc
        call    ID              ; get symbol name
        ld      de,(SYMTBL)     ; start of symbol table
        ld      c,3             ;5
        call    SYMLUK          ; lookup symbol
        pop     bc
        ld      a,0
        jr      c,S332
        inc     hl
        inc     hl
        ld      a,(hl)          ; get mode bits
        cpl
        and     EXTSYM
        jr      S332
;
;       IFB
;
ZIFB:   ld      c,0FFh
S337:   push    bc
        ld      bc,RECMAX
        call    MCHECK          ; ensure enough memory for the string
        pop     bc
        jp      c,WERROR
        call    GNC
        cp      '<'             ; angle brackets are required
        jp      nz,OERROR
        call    BACKUP
        ld      de,(SYMPTR)
        inc     de              ; leave the end marker intact
        push    de
        push    bc
        call    GETSTR          ; get the string
        pop     bc
        ex      de,hl
        pop     de
        or      a
        sbc     hl,de           ; compute string length
        ld      a,h
        or      l
        jp      S332            ; length zero means TRUE
;
;       IFNB
;
ZIFNB:  ld      c,0
        jr      S337
;
;       IFIDN
;
ZIFIDN: ld      c,0FFh
S338:   push    bc
        ld      bc,RECMAX
        call    MCHECK          ; ensure enough memory for the strings
        pop     bc
        jp      c,WERROR
        call    GNC
        cp      '<'             ; angle brackets are required
        jp      nz,OERROR
        call    BACKUP
        ld      de,(SYMPTR)
        inc     de              ; leave the end marker intact
        push    de
        push    bc
        call    GETSTR          ; get the string
        pop     bc
        xor     a
        ld      (de),a
        inc     de
        pop     hl
        call    GNC
        cp      ','
        jp      nz,OERROR
        call    GNC
        cp      '<'
        jp      nz,OERROR
        push    bc
        push    de
        push    hl
        call    BACKUP
        call    GETSTR
        xor     a
        ld      (de),a
        pop     hl
        pop     de
        pop     bc
S339:   ld      a,(de)
        ld      b,a
        sub     (hl)
        jp      nz,S332
        ld      a,b
        or      a
        jp      z,S332
        inc     hl
        inc     de
        jr      S339
;
;       IFDIF
;
ZIFDIF: ld      c,0
        jr      S338
;
;       ELSE
;
S340:   ld      a,(CLEVEL)      ; get stack depth
        or      a               ; test if empty
        jr      z,S342
        ld      hl,(CONDSP)     ; get stack pointer
        ld      a,(hl)          ; get current state
        or      a               ; test ELSE bit
        jp      m,S343          ; error if set
        cpl                     ; flip state
        inc     hl              ; point to previous state
        and     (hl)            ; include in test
        dec     hl              ; point to current state again
        or      80h             ; set ELSE bit
        ld      (hl),a          ; and save new state
        ld      a,0FFh          ; suppress IF, ELSE, ENDIF
        ld      (IFLIST),a      ;  if "NOCOND"
        ret
;
S343:   inc     hl
        ld      a,(hl)
        rrca
        and     80h             ; force error if prev cond state is true
S342:   or      'C'             ; misplaced ELSE error
        jp      S335
;
;       ENDIF
;
S350:   ld      a,(CLEVEL)      ; get stack depth
        or      a               ; test if empty
        jr      z,S352
        dec     a               ; reduce depth
        ld      (CLEVEL),a      ; save new depth
        ld      hl,(CONDSP)     ; get conditional stack pointer
        inc     hl              ; pop a state
        ld      (CONDSP),hl     ; and put back
        ld      a,0FFh          ; suppress IF, ELSE, ENDIF
        ld      (IFLIST),a      ;  if "NOCOND"
        ret                     ; finish off
;
S352:   ld      a,'B'           ; stack underflow
        jp      S335
;
