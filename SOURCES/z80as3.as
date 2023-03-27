*include	zsm.inc

	global	GNC
	global	OERROR
	global	BACKUP
	global	EVALREG
	global	REQCHR
	global	EVFLGS
	global	SAVVAL
	global	CPU
	global	EMITB
	global	VAL
	global	EVMODE
	global	REQ8U
	global	PTR1
	global	ERRFLG
	global	ID
	global	IDLEN
	global	IDBUF
	global	UCASE
	global	EVBRKT
	global	SWITCH
	global	EMITV
	global	REGVAL
	global	SAVVARS
	global	EMITSV
	global	OPCODE
	global	CHK8U
	global	EVALSRG
	global	EVALCND
	global	EVALNEW
	global	CMPHD
	global	RELERR
	global	EMITVR
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
;       SUBTTL  Instruction Class 8 - LD Instructions
;
;       Class 8 - LD instructions
;
CL8:    call    GNC
        or      a
        jp      z,OERROR
        cp      '('
        jp      z,S186          ; branch if () first operand
        cp      '<'
        jp      z,S186          ; or <>
        call    BACKUP
        call    S117            ; try special I,A and R,A cases first
        jp      nc,S120         ; branch to process I and R
        call    EVALREG         ; else get first operand
        call    REQCHR
        defb    ','
        ld      a,(EVFLGS)
        cp      RPNAME
        jp      z,S147          ; branch if rpair name
        cp      RNAME
        jp      nz,OERROR       ; error if not a reg
;
;       LD      r,...
;
        call    NOLDW           ; can't be LDW
        ld      (SAVVAL),hl     ; save reg value
        call    GNC
        or      a
        jp      z,OERROR
        cp      '('
        jp      z,S130          ; branch if () or <> second operand
        cp      '<'
        jp      z,S130
        call    BACKUP
        ld      a,(SAVVAL)
        cp      7               ; register A?
        jr      nz,S116A        ; branch if not
        call    S117            ; else check for special cases
        jr      c,S116A
        dec     c
        ld      c,57h           ; LD A,I
        jp      z,S171
        ld      c,5Fh           ; LD A,R
        jp      S171

S116A:  call    EVALREG         ; get second operand
        cp      RNAME
        jp      z,S128          ; branch if single reg
        cp      RPNAME
        jp      z,OERROR        ; register pair is illegal here
S116B:  ld      hl,(SAVVAL)
        ld      a,h
        or      a
        jr      z,S116C
        ld      a,(CPU)
        dec     a
        jp      z,OERROR        ; IXH,IXL,IYH,IYL not supported by Z180
        ld      a,h
        call    EMITB
S116C:  ld      a,l
        rlca
        rlca
        rlca
        add     a,6
        call    EMITB
        ld      hl,(VAL)
        ld      a,(EVMODE)
        call    REQ8U
        ld      a,l
        jp      EMITB           ; immed operand
;
S117:   ld      de,(PTR1)       ; save record pointer
        ld      hl,ERRFLG
        ld      c,(hl)          ; and old error flag
        call    ID              ; get identifier
        ld      (hl),c          ; ignore errors from ID
        ld      a,(IDLEN)
        dec     a               ; one char?
        jr      nz,S118         ; return with carry set if not
        ld      a,(IDBUF)
	call	UCASE
        ld      c,1
        cp      'I'
        ret     z
        inc     c
        cp      'R'
        ret     z
S118:   ld      (PTR1),de       ; restore record pointer
        scf
        ret
;
;       LD      I,A
;       LD      R,A
;
S120:   push    bc
        call    NOLDW           ; can't be LDW
        call    REQCHR
        defb    ','
        call    EVALREG
        pop     bc
        cp      RNAME
        jp      nz,OERROR
        ld      a,l
        cp      7               ; only A allowed
        jp      nz,OERROR
        dec     c
        ld      c,47h           ; ld I,A
        jr      z,S171
        ld      c,4Fh           ; ld R,A
S171:   ld      a,0EDh
        call    EMITB
        ld      a,c
        jp      EMITB
;
;       LD      r,r
;
S128:	;    equ     $               ; process rname
        ld      bc,(SAVVAL)     ; get 1st operand
        ld      a,b
        or      a
        jp      z,S128A
        ld      a,(CPU)
        dec     a
        jp      z,OERROR        ; IXH,IXL,IYH,IYL not supported by Z180
S128A:  ld      hl,(VAL)        ; get 2nd operand
        ld      a,h
        or      a
        jp      z,S128B
        ld      a,(CPU)
        dec     a
        jp      z,OERROR        ; IXH,IXL,IYH,IYL not supported by Z180
S128B:  ld      a,c
        and     6
        xor     4               ; 0 if H or L
        ld      e,a
        ld      a,l
        and     6
        xor     4
        or      e               ; 0 if both H or L
        jp      nz,S128C
        ld      a,b
        cp      h
        jp      nz,OERROR       ; can't mix HL/IX/IY in the same instruction
S128C:  ld      a,b
        or      h
        call    nz,EMITB        ; emit prefix
        ld      a,c
        rlca
        rlca
        rlca
        add     a,l
        add     a,40h           ; for r-r type inst
        jp      EMITB           ; reg-reg operand
;
;       LD      r,(...)
;
S130:   call    EVBRKT          ; evaluate ()
        ld      hl,LDTBL1
        jp      SWITCH

	psect	data

;
LDTBL1: defw    S134            ; (addr)
        defw    OERROR          ; (r)
        defw    S135            ; (rp)
        defw    S140            ; (rp+d8)
        defw    S136            ; (rp+d16)  Z280
        defw    S144            ; (x+y)     Z280
        defw    S150            ; (PC+d16)  Z280
        defw    S150            ; <addr>    Z280

	psect	text

;
;       LD      A,(addr)
;
S134:   ld      a,(SAVVAL)
        cp      07h             ; only A allowed as first operand
        jp      nz,OERROR
        ld      a,3Ah
        call    EMITB
        jp      EMITV           ; emit address
;
;       LD      r,(rp)
;
S135:   ld      a,(REGVAL)
        cp      4
        jr      z,S138          ; branch if (HL)
        cp      6
        jr      z,S142          ; branch if (SP) (Z280)
        ld      b,a
        ld      a,(SAVVAL)
        cp      7               ; else 2nd operand is (BC) or (DE)
        jp      nz,OERROR       ;  and only A is legal 1st operand
        ld      a,b
        rlca
        rlca
        rlca
        add     a,0Ah
        jp      EMITB
;
;       LD      r,(HL)
;
S138:   ld      hl,(SAVVAL)
        ld      a,h
        or      a
        jp      nz,OERROR       ; IXH,IXL,IYH,IYL not allowed
        ld      a,l
        rlca
        rlca
        rlca
        add     a,46h
        jp      EMITB
;
;       LD      r,(IX/IY+d8)
;
S140:   ld      a,(REGVAL+1)
        call    EMITB           ; emit prefix
        ld      hl,(SAVVAL)
        ld      a,h
        or      a
        jp      nz,OERROR       ; IXH,IXL,IYH,IYL not allowed
        ld      a,l
        rlca
        rlca
        rlca
        add     a,46h
        call    EMITB
        ld      a,(VAL)         ; emit displ (type/mode already checked)
        jp      EMITB
;
;       LD      r,(rp+d16)      ; Z280
;
S136:   jp	OERROR		;
;	ld      hl,(REGVAL)
;        ld      a,l
;        cp      6
;        jr      z,S142          ; jump if SP (Z280)
;        ld      a,(SAVVAL)
;        cp      7
;        jp      nz,OERROR       ; only A legal 1st operand
;        ld      a,h
;        ld      c,79h           ; IX
;        cp      0DDh
;        jr      z,S143
;        inc     c               ; IY
;        cp      0FDh
;        jr      z,S143
;        inc     c               ; HL
;S143:   ld      a,0FDh
;        call    EMITB
;        ld      a,c
;        call    EMITB
;        jp      EMITV
;
;       LD      A,(SP+dd)       ; Z280
;
S142:   jp	OERROR		;
;	ld      a,(CPU)
;        cp      2
;        jp      nz,OERROR       ; Z280 only
;        ld      a,(SAVVAL)
;        cp      7
;        jp      nz,OERROR       ; only A legal 1st operand
;        ld      a,0DDh
;        call    EMITB
;        ld      a,78h
;        call    EMITB
;        jp      EMITV
;
;       LD      A,(x+y)         ; Z280
;
S144:   jp	OERROR		;
;	ld      a,(SAVVAL)
;        cp      7
;        jp      nz,OERROR       ; only A legal 1st operand
;        ld      a,0DDh
;        call    EMITB
;        ld      a,b
;        add     a,78h
;        jp      EMITB
;
;       LD      A,<addr>        ; Z280
;       LD      A,(PC+addr)     ; Z280
;
S150:   jp	OERROR		;
;	call    NOLDW           ; LDW not allowed
;        ld      a,(SAVVAL)
;        cp      07h             ; only A allowed as first operand
;        jp      nz,OERROR
;        ld      a,0FDh
;        call    EMITB
;        ld      a,78h
;        call    EMITB
;        ld      a,c
;        cp      6
;        jp      z,EMITV         ; emit address if (PC+addr)
;        ld      de,0
;        jp      EMITVR          ; else emit relative address
;
;       LD      rp,...
;
S147:   call    GNC
        or      a
        jp      z,OERROR
        cp      '('
        jp      z,S159          ; branch if () or <> second operand
        cp      '<'
        jp      z,S159
        ld      hl,(VAL)
        ld      (SAVVAL),hl     ; save register value
        call    BACKUP
        call    EVALREG
        cp      RPNAME
        jr      z,S170          ; branch if second operand is rpair
        cp      RNAME
        jp      z,OERROR        ; single register is illegal
        ld      a,(SAVVAL+1)
        or      a
        call    nz,EMITB        ; emit prefix if first operand is index reg
        ld      a,(SAVVAL)
        rlca
        rlca
        rlca
        inc     a
        call    EMITB
        jp      EMITV           ; emit address
;
;       LD      SP,HL/IX/IY
;
S170:   ld      a,(SAVVAL)
        cp      6               ; first operand can only be SP
        jp      nz,OERROR
        ld      a,l
        cp      4               ; second can only be HL/IX/IY
        jp      nz,OERROR
        ld      a,h
        or      a
        call    nz,EMITB
        ld      a,0F9h
        jp      EMITB
;
;       LD      rp,(...)
;
S159:   ld      hl,(VAL)
        ld      (SAVVAL),hl     ; save 1st operand (rpair name)
        call    EVBRKT          ; evaluate ()
        ld      hl,LDTBL2
        jp      SWITCH

	psect 	data

;
LDTBL2: defw    S160            ; (addr)
        defw    OERROR          ; (r)
        defw    S161            ; (rp)      Z280
        defw    S162            ; (rp+d8)   Z280
        defw    S163            ; (rp+d16)  Z280
        defw    S164            ; (x+y)     Z280
        defw    S166            ; (PC+d16)  Z280
        defw    S166            ; <addr>    Z280

	psect	text
;
;       LD      rp,(addr)
;
S160:   ld      hl,(SAVVAL)     ; test 1st operand
        ld      a,l
        cp      4               ; HL,IX,IY
        jr      nz,S165         ; branch if BC,DE,SP
        ld      a,h
        or      a
        call    nz,EMITB        ; emit prefix if IX or IY
        ld      a,2Ah
        call    EMITB
        jp      EMITV           ; emit address
;
S165:   ld      a,0EDh
        call    EMITB
        ld      a,(SAVVAL)
        rlca
        rlca
        rlca
        add     a,4Bh
        call    EMITB
        jp      EMITV           ; emit address
;
;       LD      rp,(rp)         ; Z280
;
S161:   jp	OERROR		;
;	ld      a,(CPU)
;        cp      2
;        jp      nz,OERROR       ; Z280 only
;        ld      a,(REGVAL)
;        cp      6
;        ld      c,04h
;        jr      z,S163A         ; branch if (SP)
;        cp      4
;        jp      nz,OERROR       ; else must be (HL)
;
;       LD      rp,(HL)         ; Z280
;
;        ld      hl,(SAVVAL)     ; get first operand
;        ld      a,h
;        or      a
;        jp      nz,OERROR       ; can't be index register
;S161A:  ld      a,0EDh
;        call    EMITB
;        ld      a,l
;        rlca
;        rlca
;        rlca
;        add     a,06h
;        jp      EMITB
;
;       LD      rp,(IX/IY+d8)   ; Z280
;
S162:   jp	OERROR		;
;	ld      a,(CPU)
;        cp      2
;        jp      nz,OERROR       ; Z280 only
;        ld      hl,(SAVVAL)     ; get first operand
;        ld      a,h
;        or      a
;        jr      nz,S163         ; argument is d16 for index register
;        ld      a,(REGVAL+1)
;        call    EMITB           ; emit prefix
;        call    S161A           ; emit instruction bytes
;        ld      a,(VAL)
;        jp      EMITB           ; emit displacement
;
;       LD      rp,(rp+d16)     ; Z280
;
S163:   jp	OERROR		;
;	ld      hl,(REGVAL)
;        ld      a,l
;        cp      6
;        ld      c,04h
;        jr      z,S163A         ; branch if (SP)
;        ld      a,h
;        ld      c,2Ch           ; IX
;        cp      0DDh
;        jr      z,S163A
;        ld      c,34h           ; IY
;        cp      0FDh
;        jr      z,S163A
;        ld      c,3Ch           ; HL
;S163A:  ld      de,(SAVVAL)
;        ld      a,e
;        cp      4
;        jp      nz,OERROR       ; 1st operand can be only HL/IX/IY
;        ld      a,d
;        or      a
;        call    nz,EMITB        ; emit prefix if index register
;        ld      a,0EDh
;        call    EMITB
;        ld      a,c
;        call    EMITB
;        jp      EMITV
;
;       LD      rp,(x+y)        ; Z280
;
S164:   jp	OERROR		;
;	ld      de,(SAVVAL)
;        ld      a,e
;        cp      4
;        jp      nz,OERROR       ; 1st operand can be only HL/IX/IY
;        ld      a,d
;        or      a
;        call    nz,EMITB        ; emit prefix if index register
;        ld      a,0EDh
;        call    EMITB
;        ld      a,b
;        rlca
;        rlca
;        rlca
;        add     a,04h
;        jp      EMITB
;
;       LD      rp,<addr>       ; Z280
;       LD      rp,(PC+addr)    ; Z280
;
S166:   jp	OERROR		;
;	ld      hl,(SAVVAL)     ; test 1st operand
;        ld      a,l
;        cp      4
;        jp      nz,OERROR       ; only HL/IX/IY allowed
;        ld      a,h
;        or      a
;        call    nz,EMITB        ; emit prefix if IX or IY
;        ld      a,0EDh
;        call    EMITB
;        ld      a,24h
;        call    EMITB
;        ld      a,c
;        cp      6
;        jp      z,EMITV         ; emit address if (PC+addr)
;        ld      de,0
;        jp      EMITVR          ; else emit relative address
;
;       LD      (...)
;
S186:   call    EVBRKT          ; evaluate ()
        call    SAVVARS         ; SAVVAL,SAVMOD,SAVCHN <- VAL,EVMODE,EXTCHN
        ld      hl,LDTBL3
        ld      a,c
        jp      SWITCH

	psect	data

;
LDTBL3: defw    S187            ; (addr)
        defw    OERROR          ; (r)
        defw    S200            ; (rp)
        defw    S210            ; (rp+d8)
        defw    S202            ; (rp+d16)  Z280
        defw    S197            ; (x+y)     Z280
        defw    S219            ; (PC+d16)  Z280
        defw    S219            ; <addr>    Z280

	psect	text

;
;       LD      (addr),...
;
S187:   call    REQCHR
        defb    ','
        call    EVALREG
        cp      RPNAME
        jr      z,S190          ; branch if second operand rpair
        cp      RNAME
        jr      nz,S187A        ; branch if not single reg
;
;       LD      (addr),A
;
        call    NOLDW           ; can't be LDW
        ld      a,l
        cp      07h             ; only A allowed
        jp      nz,OERROR
        ld      a,32h
        call    EMITB
        jp      EMITSV          ; emit address
;
;       LD      (addr),val      ; Z280
;
S187A:  jp	OERROR		;
;	ld      a,(CPU)
;        cp      2
;        jp      nz,OERROR       ; Z280 only
;        ld      a,(OPCODE)
;        or      a
;        jr      nz,S188         ; LDW specified, use long form
;        ld      a,(EVMODE)
;        call    CHK8U
;        jr      nz,S188
;        ld      a,0DDh
;        call    EMITB
;        ld      a,03Eh
;        call    EMITB
;        call    EMITSV          ; emit address
;        ld      a,(VAL)
;        jp      EMITB           ; emit value
;
;S188:   ld      a,0DDh
;        call    EMITB
;        ld      a,11h
;        call    EMITB
;        call    EMITSV          ; emit address
;        jp      EMITV           ; emit value
;
;       LD      (addr),rp
;
S190:   ld      a,l
        cp      4               ; test for HL
        jr      nz,S196         ; branch if BC,DE,SP
        ld      a,h
        or      a
        call    nz,EMITB        ; emit prefix if IX or IY
        ld      a,22h
        call    EMITB
        jp      EMITSV          ; emit address
;
S196:   ld      a,0EDh
        call    EMITB
        ld      a,(VAL)
        rlca
        rlca
        rlca
        add     a,43h
        call    EMITB
        jp      EMITSV          ; emit address
;
;       LD      (x+y),...       ; Z280
;
S197:   jp	OERROR		;
;	call    REQCHR
;        defb    ','
;        push    bc
;        call    EVALREG
;        pop     bc
;        cp      RPNAME
;        jr      z,S198          ; branch if second operand rpair
;        cp      RNAME
;        jr      nz,S199         ; branch if not single reg
;
;       LD      (x+y),A         ; Z280
;
;        call    NOLDW           ; can't be LDW
;        ld      a,l
;        cp      07h             ; only A allowed
;        jp      nz,OERROR
;        ld      a,0EDh
;        call    EMITB
;        ld      a,b
;        rlca
;        rlca
;        rlca
;        add     a,03h
;        jp      EMITB
;
;       LD      (x+y),rp        ; Z280
;
S198:   jp	OERROR		;
;	ld      a,l
;        cp      4
;        jp      nz,OERROR       ; must be HL/IX/IY
;        ld      a,h
;        or      a
;        call    nz,EMITB        ; emit prefix if index register
;        ld      a,0EDh
;        call    EMITB
;        ld      a,b
;        rlca
;        rlca
;        rlca
;        add     a,05h
;        jp      EMITB
;
;       LD      (x+y),n         ; Z280
;
S199:   jp	OERROR		;
;	call    NOLDW           ; can't be LDW
;        ld      a,0DDh
;        call    EMITB
;        ld      a,b
;        rlca
;        rlca
;        rlca
;        add     a,06h
;        call    EMITB
;        ld      hl,(VAL)
;        ld      a,(EVMODE)
;        call    CHK8U
;        call    nz,VALERR       ; must be 8-bit value
;        ld      a,l
;        jp      EMITB
;
;       LD      (rp),...
;
S200:   call    SAVVARS         ; SAVVAL,SAVMOD,SAVCHN <- VAL,EVMODE,EXTCHN
        call    REQCHR
        defb    ','
        call    EVALREG         ; evaluate second operand
        ld      a,(REGVAL)
        cp      4
        jp      z,S204          ; branch if first operand is (HL)
        cp      6
        jp      z,S201          ; branch if (SP) (Z280)
;
;       LD      (rp),A
;
        call    NOLDW           ; LDW not allowed
        ld      a,(EVFLGS)
        cp      RNAME
        jp      nz,OERROR
        ld      a,(VAL)
        cp      7               ; only reg A allowed as 2nd operand
        jp      nz,OERROR
        ld      a,(REGVAL)
        cp      2+1
        jp      nc,OERROR       ; only BC and DE allowed as 1st operand
        rlca
        rlca
        rlca
        add     a,2
        jp      EMITB
;
;       LD      (rp+d16),...    ; Z280
;
S202:   jp	OERROR		;
;	call    SAVVARS         ; SAVVAL,SAVMOD,SAVCHN <- VAL,EVMODE,EXTCHN
;        call    REQCHR
;        defb    ','
;        call    EVALREG         ; evaluate second operand
;        ld      hl,(REGVAL)
;        ld      a,l
;        cp      6
;        jp      z,S201          ; branch if 1st operand (SP) (Z280)
;        ld      a,(EVFLGS)      ; test 2nd operand
;        cp      RNAME
;        jr      z,S203A         ; branch if single reg
;        cp      RPNAME
;        jr      z,S203B         ; branch if rpair
;
;       LD      (rp+d16),n8
;
;        call    NOLDW           ; 8-bit immediate value, LDW not allowed
;        ld      a,(REGVAL+1)
;        cp      0DDh
;        ld      c,0Eh           ; IX
;        jr      z,S203C
;        cp      0FDh
;        ld      c,16h           ; IY
;        jr      z,S203C
;        ld      c,1Eh           ; HL
;S203C:  ld      a,0FDh
;S213C:  call    EMITB
;        ld      a,c
;        call    EMITB
;        call    EMITSV          ; emit displacement
;        ld      hl,(VAL)
;        ld      a,(EVMODE)
;        call    REQ8U
;        ld      a,l
;        jp      EMITB
;
;       LD      (rp+d16),A      ; Z280
;
S203A:  jp	OERROR		;
;	call    NOLDW           ; LDW not allowed
;        ld      a,(VAL)
;        cp      7
;        jp      nz,OERROR       ; only A is legal
;        ld      a,(REGVAL+1)
;        cp      0DDh
;        ld      c,2Bh           ; IX
;        jr      z,S203D
;        cp      0FDh
;        ld      c,33h           ; IY
;        jr      z,S203D
;        ld      c,3Bh           ; HL
;S203D:  ld      a,0EDh
;        call    EMITB
;        ld      a,c
;        call    EMITB
;        jp      EMITSV
;
;       LD      (rp+d16),rp     ; Z280
;
;S203B:  ld      hl,(VAL)
;        ld      a,l
;        cp      4
;        jp      nz,OERROR       ; only HL/IX/IY allowed
;        ld      a,h
;        or      a
;        call    nz,EMITB        ; emit prefix if index register
;S203F:  ld      a,(REGVAL+1)
;        cp      0DDh
;        ld      c,2Dh           ; IX
;        jr      z,S203E
;        cp      0FDh
;        ld      c,35h           ; IY
;        jr      z,S203E
;        ld      c,3Dh           ; HL
;S203E:  ld      a,0EDh
;        call    EMITB
;        ld      a,c
;        call    EMITB
;        jp      EMITSV
;
;       LD      (SP+d16),...    ; Z280 only
;
S201:   jp	OERROR		;
;	ld      a,(CPU)
;        cp      2
;        jp      nz,OERROR       ; Z280 only
;        ld      a,(EVFLGS)      ; test 2nd operand
;        cp      RNAME
;        jr      z,S201A         ; branch if single reg
;        cp      RPNAME
;        jr      z,S201B         ; branch if rpair
;
;       LD      (SP+d16),n8
;
;        call    NOLDW           ; 8-bit immediate value, LDW not allowed
;        ld      c,06h
;        ld      a,0DDh
;        jp      S213C
;
;       LD      (SP+d16),A      ; Z280
;
;S201A:  call    NOLDW           ; LDW not allowed
;        ld      a,(VAL)
;        cp      7
;        jp      nz,OERROR       ; only A is legal
;        ld      c,03h
;        jp      S203E
;
;       LD      (SP+d16),rp     ; Z280
;
;S201B:  ld      hl,(VAL)
;        ld      a,l
;        cp      4
;        jp      nz,OERROR       ; only HL/IX/IY allowed
;        ld      a,h
;        or      a
;        call    nz,EMITB        ; emit prefix if index register
;        ld      c,05h
;        ld      a,0EDh
;        jp      S203E
;
;       LD      (HL),...
;
S204:   ld      a,(EVFLGS)
        cp      RNAME
        jr      z,S209          ; branch if single register
        cp      RPNAME
        jr      z,S208          ; branch if rpair (Z280)
;
;       LD      (HL),n
;
        ld      a,(OPCODE)
        or      a
        jr      nz,S206         ; LDW specified, use long form
        ld      a,(CPU)
        cp      2
        jr      nz,S205         ; branch if Z80/Z180 mode
        ld      a,(EVMODE)
        ld      hl,(VAL)
        call    CHK8U           ; test for short/long form
        jr      z,S205          ; branch if short
;
;       LD      (HL),n16        ; Z280
;
S206:   ld      a,0DDh
        call    EMITB
        ld      a,01h
        call    EMITB
        jp      EMITV
;
;       LD      (HL),n8
;
S205:   ld      a,36h
        call    EMITB
        ld      hl,(VAL)
        ld      a,(EVMODE)
        call    REQ8U
        ld      a,l
        jp      EMITB
;
;       LD      (HL),rp         ; Z280
;
S208:   jp	OERROR		;
;	ld      a,(CPU)
;        cp      2
;        jp      nz,OERROR       ; Z280 only
;        ld      a,h
;        or      a
;        jp      nz,OERROR       ; index register not allowed
;S208A:  ld      a,0EDh
;        call    EMITB
;        ld      a,l
;        rlca
;        rlca
;        rlca
;        add     a,0Eh
;        jp      EMITB
;
;       LD      (HL),r
;
S209:   call    NOLDW           ; LDW not allowed
        ld      hl,(VAL)
        ld      a,h
        or      a
        jp      nz,OERROR       ; IXH,IXL,IYH,IYL not allowed
        ld      a,l
        add     a,70h
        jp      EMITB
;
;       LD      (IX/IY+d8),...
;
S210:   ld      hl,(VAL)
        ld      (SAVVAL),hl     ; save index
        call    REQCHR
        defb    ','
        call    EVALREG         ; evaluate second operand
        cp      RPNAME
        jr      z,S215          ; branch if rpair (Z280)
        cp      RNAME
        jr      z,S218          ; branch if single register
        call    NOLDW           ; LDW not allowed (8-bit operand)
        ld      a,(REGVAL+1)
        call    EMITB           ; emit prefix
        ld      a,36h
        call    EMITB
        ld      a,(SAVVAL)
        call    EMITB           ; output index (type/mode already checked)
        ld      hl,(VAL)
        ld      a,(EVMODE)
        call    REQ8U
        ld      a,l
        jp      EMITB           ; output byte value
;
;       LD      (IX/IY+d8),rp   ; Z280
;
S215:   jp	OERROR		;
;	ld      a,(CPU)
;        cp      2
;        jp      nz,OERROR       ; Z280 only
;        ld      a,h
;        or      a
;        jp      nz,S203B        ; use d16 form if 2nd operand is index register
;        ld      a,(REGVAL+1)
;        call    EMITB           ; emit prefix
;        call    S208A
;        jr      S218A
;
;       LD      (IX/IY+d8),r
;
S218:   call    NOLDW           ; LDW not allowed
        ld      a,(REGVAL+1)
        call    EMITB           ; emit prefix
        ld      hl,(VAL)
        ld      a,h
        or      a
        jp      nz,OERROR       ; IXH,IXL,IYH,IYL not allowed
        ld      a,l
        add     a,70h
        call    EMITB
S218A:  ld      a,(SAVVAL)
        jp      EMITB           ; output index (type/mode already checked)
;
;       LD      <addr>,...      ; Z280
;       LD      (PC+addr),...   ; Z280
;
S219:   jp	OERROR		;
;	push    bc              ; remember <addr> or (PC+addr) code
;        call    REQCHR
;        defb    ','
;        call    EVALREG
;        pop     bc
;        cp      RPNAME
;        jr      z,S219A         ; branch if second operand rpair
;        cp      RNAME
;        jr      nz,S219B        ; branch if not single reg
;
;       LD      <addr>,A        ; Z280
;       LD      (PC+addr),A     ; Z280
;
;        call    NOLDW           ; can't be LDW
;        ld      a,l
;        cp      07h             ; only A allowed
;        jp      nz,OERROR
;        ld      a,0EDh
;        call    EMITB
;        ld      a,23h
;        call    EMITB
;        ld      a,c
;        cp      6
;        jp      z,EMITSV        ; emit address if (PC+addr)
;        ld      de,0
;        jp      EMITSR          ; else emit relative address
;
;       LD      <addr>,val      ; Z280
;       LD      (PC+addr),val   ; Z280
;
;S219B:  ld      a,(OPCODE)
;        or      a
;        jr      nz,S219E        ; LDW specified, use long form
;        ld      a,(EVMODE)
;        call    CHK8U           ; else check operand size
;        jr      nz,S219E        ; branch if long
;        ld      a,0FDh          ; else use short form
;        call    EMITB
;        ld      a,06h
;        call    EMITB
;        ld      a,c
;        cp      6
;        jr      z,S219C         ; branch if (PC+addr)
;        ld      de,-1
;        call    EMITSR          ; emit relative address if <addr>
;        jr      S219D
;S219C:  call    EMITSV          ; else emit unmodified address
;S219D:  ld      a,(VAL)
;        jp      EMITB
;
;S219E:  ld      a,0DDh
;        call    EMITB
;        ld      a,31h
;        call    EMITB
;        ld      a,c
;        cp      6
;        jr      z,S219F         ; branch if (PC+addr)
;        ld      de,-2
;        call    EMITSR          ; emit relative address if <addr>
;        jp      EMITV
;S219F:  call    EMITSV          ; else emit unmodified address
;        jp      EMITV
;
;       LD      <addr>,rp       ; Z280
;       LD      (PC+addr),rp    ; Z280
;
;S219A:  ld      a,l
;        cp      4
;        jp      nz,OERROR       ; only HL/IX/IY allowed
;        ld      a,h
;        or      a
;        call    nz,EMITB        ; emit prefix if index register
;        ld      a,0EDh
;        call    EMITB
;        ld      a,25h
;        call    EMITB
;        ld      a,c
;        cp      6
;        jp      z,EMITSV        ; emit address if (PC+addr)
;        ld      de,0
;        jp      EMITSR          ; else emit relative address
;
;       Set syntax error flag if the instruction is LDW
;
NOLDW:  ld      a,(OPCODE)
        or      a
        ret     z
        jp      OERROR
;
;       SUBTTL  Instruction Class 9 - PUSH & POP
;
;       Class 9 - PUSH, POP
;
CL9:    call    GNC
        or      a
        jp      z,OERROR
        cp      '('
        jr      z,S225          ; branch if () form (Z280)
        cp      '<'
        jp      z,S227          ; branch if <> form (Z280)
        call    BACKUP
        call    EVALSRG         ; include special regs
        cp      RSNAME
        jr      nz,S221
        ld      a,l
        cp      6
        jp      nz,OERROR       ; must be AF
        jr      S222
S221:   cp      RPNAME
        jp      nz,S223         ; branch if not rpair (Z280)
        ld      a,l
        cp      6
        jp      z,OERROR        ; can't be SP
        ld      a,h
        or      a
        call    nz,EMITB        ; emit prefix if index register
S222:   ld      a,l
        rlca
        rlca
        rlca
        ld      b,a
        ld      a,(OPCODE)
        add     a,b
        jp      EMITB
;
S223:   cp      RNAME
        jp      z,OERROR        ; can't be single reg
        ld      a,(CPU)
        cp      2
        jp      nz,OERROR       ; Z280 only
        ld      a,(OPCODE)
        cp      0C1h
        jp      z,OERROR        ; PUSH only
        ld      a,0FDh
        call    EMITB
        ld      a,0F5h
        call    EMITB
        jp      EMITV
;
;       PUSH    (rp)            ; Z280
;
S225:   jp	OERROR		;
;	ld      a,(CPU)
;        cp      2
;        jp      nz,OERROR       ; Z280 only
;        call    EVALSRG
;        cp      RSNAME
;        jr      z,S228          ; branch if special reg
;        cp      RNAME
;        jp      z,OERROR        ; can't be single reg
;        cp      RPNAME
;        jr      nz,S226
;        ld      de,4
;        call    CMPHD           ; only (HL) allowed
;        jp      nz,OERROR
;        ld      a,0DDh
;        call    EMITB
;        ld      a,(OPCODE)
;        call    EMITB
;        call    REQCHR
;        defb    ')'
;        ret
;
;       PUSH    (addr)          ; Z280
;
;S226:   ld      a,0DDh
;        call    EMITB
;        ld      a,(OPCODE)
;        add     a,10h
;        call    EMITB
;        call    EMITV
;        call    REQCHR
;        defb    ')'
;        ret
;
;       PUSH    <addr>          ; Z280
;
S227:   jp	OERROR		;
;	ld      a,(CPU)
;        cp      2
;        jp      nz,OERROR
;        call    EVALNEW
;        ld      a,0DDh
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
;       PUSH    (PC+addr)       ; Z280
;
;S228:   ld      a,(CPU)
;        cp      2
;        jp      nz,OERROR
;        ld      a,l
;        cp      3
;        jp      nz,OERROR       ; only PC allowed
;        call    EVALNEW
;        ld      a,0DDh
;        call    EMITB
;        ld      a,(OPCODE)
;        add     a,30h
;        call    EMITB
;        call    EMITV           ; emit address
;        call    REQCHR
;        defb    ')'
;        ret

;       SUBTTL  Instruction Class 10 - Exchange Instructions
;
;       Class 10 - Exchange (EX)
;
CL10:   call    GNC
        or      a
        jp      z,OERROR
        cp      '('
        jp      z,S235          ; branch if () form
        call    BACKUP
        call    EVALSRG         ; get first operand, include special regs
        cp      RPNAME
        jr      nz,S230         ; branch if not rpair
        ld      a,h
        or      a
        jr      z,S231
        ld      a,(CPU)         ; index register allowed only for Z280
        cp      2
        jp      nz,OERROR
        ld      a,h
        call    EMITB
        jr      S232
S231:   ld      a,l
        cp      2               ; else must be DE
        jp      nz,OERROR
S232:   call    REQCHR
        defb    ','
        call    EVALREG         ; get second operand
        cp      RPNAME
        jp      nz,OERROR       ; must be rpair
        ld      a,l
        cp      4               ; must be HL
        jp      nz,OERROR
        ld      a,h
        or      a
        jp      nz,OERROR
        ld      a,0EBh
        jp      EMITB
;
S230:   cp      RNAME
        jr      z,S236          ; branch if single register (Z280)
        call    S233            ; must be AF
        jp      nz,OERROR
        call    REQCHR
        defb    ','
        call    EVALSRG
        call    S233            ; second operand must be AF'
        jp      nz,OERROR
        ld      hl,(PTR1)
        ld      a,(hl)
        cp      27H             ;'
        jp      nz,OERROR
        inc     hl
        ld      (PTR1),hl
        ld      a,8
        jp      EMITB
;
S233:   cp      RSNAME
        ret     nz
        ld      a,l
        cp      6               ; if AF, return Z=1
        ret
;
S235:   call    EVALREG
        cp      RPNAME
        jp      nz,OERROR       ; must be rpair
        ld      a,l
        cp      6
        jp      nz,OERROR       ; only (SP) allowed
        call    REQCHR
        defb    ')'
        call    REQCHR
        defb    ','
        call    EVALREG
        cp      RPNAME
        jp      nz,OERROR
        ld      a,l
        cp      4               ; only HL,IX,IY allowed
        jp      nz,OERROR
        ld      a,h
        or      a
        call    nz,EMITB        ; emit prefix if index register
        ld      a,0E3h
        jp      EMITB
;
S236:   jp	OERROR		;
;	ld      a,(CPU)
;        cp      2
;        jp      nz,OERROR       ; Z280 only
;        call    REQCHR
;        defb    ','
;        ld      hl,(VAL)
;        ld      a,l
;        cp      7
;        jr      z,S237          ; branch if A
;        cp      4
;        jp      nz,OERROR
;        ld      a,h
;        or      a
;        jp      nz,OERROR       ; else only H allowed
;        call    EVALREG
;        cp      RNAME
;        jp      nz,OERROR
;        ld      a,l
;        cp      5
;        jp      nz,OERROR
;        ld      a,h
;        or      a
;        jp      nz,OERROR       ; second operand must be L
;        ld      a,0EDh
;        call    EMITB
;        ld      a,0EFh
;        jp      EMITB
;
;       EX      A,...           ; Z280
;
;S237:   call    GNC
;        or      a
;        jp      z,OERROR
;        cp      '('
;        jr      z,S238          ; branch if () form
;        cp      '<'
;        jp      z,S238          ; branch if <> form
;        call    BACKUP
;        call    EVALREG
;        cp      RNAME
;        jp      nz,OERROR       ; error if not single register
;        ld      a,h
;        or      a
;        call    nz,EMITB
;        ld      a,0EDh
;        call    EMITB
;        ld      a,(VAL)
;        rlca
;        rlca
;        rlca
;        add     a,07h
;        jp      EMITB
;
;       EX      A,(...)
;
;S238:   call    EVBRKT          ; evaluate ()
;        ld      hl,EXTBL
;        jp      SWITCH

;	psect	data

;
;EXTBL:  defw    S238M           ; (addr)
;        defw    OERROR          ; (r)
;        defw    S238A           ; (rp)
;        defw    S238B           ; (rp+d8)
;        defw    S238E           ; (rp+d16)
;        defw    S238D           ; (x+y)
;        defw    S239            ; (PC+d16)
;        defw    S239            ; <addr>

;	psect	text

;
;       EX      A,(addr)        ; Z280
;
;S238M:  ld      a,0DDh
;        call    EMITB
;        ld      a,0EDh
;        call    EMITB
;        ld      a,3Fh
;        call    EMITB
;        jp      EMITV           ; emit address
;
;       EX      A,(rp)          ; Z280
;
;S238A:  ld      a,(REGVAL)
;        cp      6
;        jp      z,S238C         ; branch if SP
;        cp      4
;        jp      nz,OERROR       ; else must be HL/IX/IY
;
;       EX      A,(HL)          ; Z280
;
;        ld      a,0EDh          ; else use short form
;        call    EMITB
;        ld      a,37h
;        jp      EMITB
;
;       EX      A,(IX/IY+d8)
;
;S238B:  ld      a,(REGVAL+1)
;        call    EMITB           ; emit prefix
;        ld      a,0EDh
;        call    EMITB
;        ld      a,37h
;        call    EMITB
;        ld      a,(VAL)
;        jp      EMITB           ; output index (type/mode already checked)
;
;       EX      A,(rp+d16)
;
;S238E:  ld      hl,(REGVAL)
;        ld      a,l
;        cp      6
;        jr      z,S238C         ; branch if (SP)
;        ld      a,h
;        cp      0DDh
;        ld      c,08h           ; IX
;        jr      z,S238H
;        cp      0FDh
;        ld      c,10h           ; IY
;        jr      z,S238H
;        ld      c,18h           ; HL
;S238H:  ld      a,0FDh
;        call    EMITB
;        ld      a,0EDh
;        call    EMITB
;        ld      a,c
;        add     a,07h
;        call    EMITB
;        jp      EMITV           ; emit address
;
;       EX      A,(SP+d16)      ; Z280
;
;S238C:  ld      b,00h
;        call    S238D
;        jp      EMITV           ; emit address
;
;       EX      A,(x+y)         ; Z280
;
;S238D:  ld      a,0DDh
;        call    EMITB
;        ld      a,0EDh
;        call    EMITB
;        ld      a,b
;        rlca
;        rlca
;        rlca
;        add     a,07h
;        jp      EMITB
;
;       EX      A,<addr>        ; Z280
;       EX      A,(PC+addr)     ; Z280
;
;S239:   ld      a,0FDh
;        call    EMITB
;        ld      a,0EDh
;        call    EMITB
;        ld      a,07h
;        call    EMITB
;        ld      a,c
;        cp      6
;        jp      z,EMITV         ; emit address if (PC+addr)
;        ld      de,0
;        jp      EMITVR          ; else emit relative address

;       SUBTTL  Instruction Class 11 - Returns
;
;       Class 11 - RETURNS
;
CL11:   call    GNC
        or      a
        jr      z,S248          ; branch if no operand
        call    BACKUP
        call    EVALCND
        cp      CONDD
        jp      nz,OERROR
        ld      a,(VAL)
        rlca
        rlca
        rlca
        and     38h
        add     a,0C0h
        jp      EMITB
;
S248:   ld      a,(OPCODE)
        jp      EMITB

;       SUBTTL  Instruction Class 12 - Bit Manipulation
;
;       Class 12 - BIT, SET, RES
;
CL12:   call    EVALNEW         ; get bit no
        ld      de,7            ; max bit number is 7
        call    CMPHD
        jp      c,OERROR
        ld      a,(EVMODE)
        or      a
        jp      nz,RELERR
        ld      (SAVVAL),hl     ; save bit number
        call    REQCHR
        defb    ','             ; ensure legal separator
        call    GNC
        or      a
        jp      z,OERROR
        cp      '('
        jp      z,S257          ; jump if () form
        call    BACKUP
        call    EVALREG         ; get register
        cp      RNAME           ; must be single register
        jp      nz,OERROR
        ld      a,0CBh
        call    EMITB
        ld      a,(SAVVAL)
        rlca
        rlca
        rlca
        ld      b,a
        ld      a,(OPCODE)
        add     a,b
        ld      b,a
        ld      a,(VAL)
        add     a,b
        jp      EMITB
;
S257:   call    EVALREG
        cp      RPNAME
        jp      nz,OERROR       ; must be register pair
        ld      a,l
        cp      4
        jp      nz,OERROR       ; only HL,IX,DE allowed
        ld      a,h
        or      a
        jr      nz,S260         ; branch if index register
        ld      a,0CBh
        call    EMITB
        ld      a,(SAVVAL)
        rlca
        rlca
        rlca
        add     a,6
        ld      b,a
        ld      a,(OPCODE)
        add     a,b
        call    EMITB
        call    REQCHR
        defb    ')'             ; bypass )
        ret

S260:   call    EMITB
        call    EVALNEW         ; get index
        call    REQCHR
        defb    ')'             ; bypass )
        ld      a,0CBh
        call    EMITB
        ld      a,(EVMODE)
        call    REQ8U           ; ensure 8-bit index (REQ8S?)
        ld      a,l
        call    EMITB
        ld      a,(SAVVAL)
        rlca
        rlca
        rlca
        add     a,6
        ld      b,a
        ld      a,(OPCODE)
        add     a,b
        jp      EMITB
;
;       SUBTTL  Instruction Class 13 - INC & DEC
;
;       Class 13 - INC, DEC
;
CL13:   call    GNC
        or      a
        jp      z,OERROR
        cp      '('
        jp      z,S290          ; branch if () form
        cp      '<'
        jp      z,S290          ; branch if <> form
        call    BACKUP
        call    EVALREG
        cp      RNAME
        jr      z,S280          ; branch if single register
        cp      RPNAME
        jp      nz,OERROR
S270:   ld      a,h
        or      a
        jr      nz,S276         ; branch if index register
        ld      a,l
        rlca
        rlca
        rlca
        ld      b,a
        ld      a,(OPCODE+1)
        add     a,b
        jp      EMITB
;
;       INC     IX/IY
;
S276:   call    EMITB           ; emit prefix
        ld      a,(OPCODE+1)
        add     a,20h
        jp      EMITB
;
;       INC     r
;
S280:   ld      hl,(VAL)
        ld      a,h
        or      a
        jr      z,S280B
        ld      a,(CPU)
        dec     a
        jp      z,OERROR        ; IXH,IXL,IYH,IYL not supported by Z180
        ld      a,h
        call    EMITB           ; emit prefix
S280B:  ld      a,l
        rlca
        rlca
        rlca
        ld      b,a
        ld      a,(OPCODE)
        add     a,b
        jp      EMITB
;
;       INC     (...)
;
S290:   call    EVBRKT          ; evaluate ()
        ld      hl,INCTBL
        jp      SWITCH

	psect	data
;
INCTBL: defw    S295            ; (addr)
        defw    OERROR          ; (r)
        defw    S291            ; (rp)
        defw    S294            ; (rp+d8)
        defw    S297            ; (rp+d16)
        defw    S292            ; (x+y)
        defw    S271            ; (PC+d16)
        defw    S271            ; <addr>

	psect	text

;
;       INC     (addr)          ; Z280
;
S295:   jp	OERROR		;
;	ld      a,(CPU)
;        cp      2
;        jp      nz,OERROR       ; Z280 only
;        ld      c,38h
;        jp      S296A
;
;       INC     (rp)
;
S291:   ld      a,(REGVAL)
        cp      6
        jr      z,S296          ; branch if (SP) (Z280)
        cp      4
        jp      nz,OERROR       ; error if not (HL)
;
;       INC     (HL)
;
        ld      a,(OPCODE)
        add     a,30h
        jp      EMITB
;
;       INC     (IX/IY+d8)
;
S294:   ld      a,(REGVAL+1)
        call    EMITB           ; emit index register prefix
        ld      a,(OPCODE)
        add     a,30h
        call    EMITB
        ld      a,(VAL)
        jp      EMITB
;
;       INC     (rp+d16)
;
S297:   ld      hl,(REGVAL)
        ld      a,l
        cp      6
        jr      z,S296          ; branch if (SP)
        ld      a,0FDh
        call    EMITB
        ld      a,(REGVAL+1)
        ld      c,01h           ; IX
        cp      0DDh
        jr      z,S293
        inc     c               ; IY
        cp      0FDh
        jr      z,S293
        inc     c               ; HL
S293:   ld      a,c
        rlca
        rlca
        rlca
        ld      c,a
        ld      a,(OPCODE)
        add     a,c
        call    EMITB
        jp      EMITV           ; emit address
;
;       INC     (SP+nnnn)       ; Z280
;
S296:   jp	OERROR		;
;	ld      a,(CPU)
;        cp      2
;        jp      nz,OERROR       ; Z280 only
;        ld      c,00h
;S296A:  ld      a,0DDh
;        call    EMITB
;        ld      a,(OPCODE)
;        add     a,c
;        call    EMITB
;        jp      EMITV           ; emit address
;
;       INC     (x+y)           ; Z280
;
S292:   ld      a,0DDh
        call    EMITB
        ld      a,b
        rlca
        rlca
        rlca
        ld      c,a
        ld      a,(OPCODE)
        add     a,c
        jp      EMITB
;
;       INC     <addr>          ; Z280
;       INC     (PC+addr)       ; Z280
;
S271:   ld      a,0FDh
        call    EMITB
        ld      a,(OPCODE)
        call    EMITB
        ld      a,c
        cp      6
        jp      z,EMITV         ; emit address if (PC+addr)
        ld      de,0
        jp      EMITVR          ; else emit relative address

;       SUBTTL  Instruction Class 14 - MLT, TST
;
;       Class 14 - Extra Z180 instructions: MLT, TST
;
CL14:   
;	ld      a,(OPCODE)
;        or      a
;        jr      nz,S452         ; branch if TST
;        call    EVALREG
;        cp      RPNAME          ; ensure rpair
;        jp      nz,OERROR
;        ld      a,0EDh
;        call    EMITB
;        ld      a,(VAL)
;        rlca
;        rlca
;        rlca
;        and     30h
;        add     a,4Ch
;        jp      EMITB
;
;       TST
;
;S452:   ld      a,0EDh
;        call    EMITB
;        call    GNC
;        or      a
;        jp      z,OERROR
;        cp      '('
;        jr      z,S454          ; branch if '(' form
;        call    BACKUP
;        call    EVALREG
;        cp      RNAME
;        jr      z,S453          ; branch if single register
;        cp      RPNAME          ; register pair is illegal
;        jp      z,OERROR
;
;       TST     nn
;
;        ld      a,64h
;        call    EMITB
;        ld      hl,(VAL)
;        ld      a,(EVMODE)
;        call    REQ8U
;        ld      a,l
;        jp      EMITB
;
;       TST     r
;
;S453:   ld      a,(VAL)
;        rlca
;        rlca
;        rlca
;        add     a,04h
;        jp      EMITB
;
;       TST     (HL)
;
;S454:   call    EVALREG
;        cp      RPNAME
;        jp      nz,OERROR       ; must be rpair
;        ld      de,4
;        call    CMPHD           ; only HL allowed
;        jp      nz,OERROR
;        call    REQCHR
;        defb    ')'
;        ld      a,34h
;        jp      EMITB
;
;       SUBTTL  Instruction Class 15 - ADDW, CPW, SUBW, MULTW
;
;       Class 15 - ADDW, CPW, SUBW, MULTW, MULTUW (Z280)
;
CL15:   
;	call    GNC
;        or      a
;        jp      z,OERROR
;        cp      '('
;        jr      z,S461          ; branch if () form
;        cp      '<'
;        jp      z,S464          ; branch if <> form
;        call    BACKUP
;        call    EVALREG
;        cp      RPNAME
;        jp      nz,S460A        ; branch if not rpair
;        ld      c,a
;        ld      de,4
;        call    CMPHD
;        ld      a,c
;        jp      nz,S460A        ; branch if not HL
;        call    GNC
;        cp      ','
;        jr      z,S460B         ; branch if it was the optional HL
;        or      a
;        jr      z,S462          ; else branch to process HL as 2nd operand
;        jp      OERROR
;S460B:  call    GNC
;        or      a
;        jp      z,OERROR
;        cp      '('
;        jr      z,S461          ; branch if () form
;        cp      '<'
;        jp      z,S464          ; branch if <> form
;        call    BACKUP
;        call    EVALREG         ; get second operand
;S460A:  cp      RNAME
;        jp      z,OERROR        ; can't be single register
;        cp      RPNAME
;        jr      z,S462          ; branch if rpair
;        ld      a,0FDh
;        ld      b,30h
;S460:   call    EMITB
;        ld      a,0EDh
;        call    EMITB
;        ld      a,(OPCODE)
;        add     a,b
;        call    EMITB
;        jp      EMITV           ; emit address
;
;       ADDW    HL,rpair
;
;S462:   ld      a,h
;        or      a
;        call    nz,EMITB
;        ld      a,0EDh
;        call    EMITB
;        ld      a,(OPCODE)
;        ld      b,a
;        ld      a,(VAL)
;        rlca
;        rlca
;        rlca
;        add     a,b
;        jp      EMITB
;
;       ADDW    HL,(...)
;
;S461:   call    EVALSRG
;        cp      RSNAME
;        jr      z,S466          ; branch if special reg
;        cp      RNAME
;        jp      z,OERROR
;        cp      RPNAME
;        jr      nz,S463         ; branch if not rpair
;        ld      a,l
;        cp      4
;        jp      nz,OERROR       ; only HL, IX and IY allowed
;        ld      a,h
;        or      a
;        jr      z,S465          ; branch if HL
;        rrca
;        and     10h             ; DD -> 00, FD -> 10
;        ld      b,a
;        push    bc
;        call    EVALNEW
;        call    REQCHR
;        defb    ')'
;        pop     bc
;        ld      a,0FDh
;        jr      S460
;
;       ADDW    HL,(HL)
;
;S465:   call    REQCHR
;        defb    ')'
;        ld      a,0DDh
;        call    EMITB
;        ld      a,0EDh
;        call    EMITB
;        ld      a,(OPCODE)
;        jp      EMITB
;
;S463:   call    REQCHR
;        defb    ')'
;        ld      a,0DDh
;        ld      b,10h
;        jr      S460
;
;       ADDW    HL,<addr>
;
;S464:   call    EVALNEW
;        call    REQCHR
;        defb    '>'
;        ld      a,0DDh
;        call    EMITB
;        ld      a,0EDh
;        call    EMITB
;        ld      a,(OPCODE)
;        add     a,30h
;        call    EMITB
;        ld      de,0
;        jp      EMITVR          ; emit relative address
;
;       ADDW    HL,(PC+addr)
;
;S466:   ld      a,l
;        cp      3
;        jp      nz,OERROR       ; special reg can only be PC
;        call    EVALNEW         ; get index
;        call    REQCHR
;        defb    ')'
;        ld      a,0DDh
;        ld      b,30h
;        jp      S460
