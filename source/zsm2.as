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

*include        ZSM.INC

;local
        global  VAL
        global  ERRFLG,BACKUP,GNC,CPU
        global  PCFLAG,CURSEG
        global  OERROR,EVMODE
        global  GETPPC
        global  REQCHR

;external
        global  CMPHD
        global  EVAL,EVALNEW,EVALREG
        global  EVALCND,EVBRKT
        global  EVALSRG
        global  RELERR,REGVAL

        global  PASSNO,MODNAM,MODIDN    ;,CSSIZE,DSSIZE
        global  EFLG,ENDADR,ENDMOD	;,PHFLAG,UPDSIZ,SELCMN
        global  QFLAG,COMNTC,LOCFLG,ASEGPC,CSEGPC,DSEGPC
        global  LCLNUM,OPCODE,TYPTBL

        global  EMITB,REQ8U,EMITV,EMITVR,SWITCH

        global  TYPTBL,CL1,CL2,CL3,CL4,CL5,CL6,CL7,CL8,CL9
        global  CL10,CL11,CL12,CL13,CL14,CL15,CL16,CL17,CL18,CL19
        global  CL20,CL21,CL22,CL23,CL24,CL25,CL26,CL27

        psect   text

;**********************************************************************;
;-----------------------------------------------------------------------
;
;       Instruction class dispatch table
;
TYPTBL: defw    CL1     ; Class  1 - Opcode only
        defw    CL2     ; Class  2 - Rotates
        defw    CL3     ; Class  3 - Jumps (non relative) calls
        defw    CL4     ; Class  4 - Relative jumps (JR and DJNZ)
        defw    CL5     ; Class  5 - RST
        defw    CL6     ; Class  6 - Arithmetic instructions
        defw    CL7     ; Class  7 - I/O
        defw    CL8     ; Class  8 - LD instructions
        defw    CL9     ; Class  9 - PUSH, POP
        defw    CL10    ; Class 10 - Exchange (EX)
        defw    CL11    ; Class 11 - Returns
        defw    CL12    ; Class 12 - BIT, SET, RES
        defw    CL13    ; Class 13 - INC, DEC
        defw    CL14    ; Class 14 - MLT, TST (Z180)
        defw    CL15    ; Class 15 - ADDW, CPW, SUBW, MULTW, MULTUW (Z280)
        defw    CL16    ; Class 16 - INCW, DECW (Z280)
        defw    CL17    ; Class 17 - DIV, DIVU (Z280)
        defw    CL18    ; Class 18 - DIVW, DIVUW (Z280)
        defw    CL19    ; Class 19 - LDA (Z280)
        defw    CL20    ; Class 20 - LDCTL (Z280)
        defw    CL21    ; Class 21 - LDUD, LDUP (Z280)
        defw    CL22    ; Class 22 - MULT, MULTU (Z280)
        defw    CL23    ; Class 23 - CPL, NEG, EXTS
        defw    CL24    ; Class 24 - DI, EI
        defw    CL25    ; Class 25 - SC (Z280)
        defw    CL26    ; Class 26 - EPUM, MEPU (Z280)
        defw    CL27    ; Class 27 - Pseudo operators

;       SUBTTL  Instruction Class 1 - Opcode only
;
;       Class 1 - Opcode only
;
CL1:    ld      hl,(OPCODE)
        ld      a,l
        call    EMITB
        ld      a,h
        cp      0FFh            ; IM set?
        jr      z,S18
        or      a
        call    nz,EMITB        ; emit second byte, if any
        ret
;
S18:    call    GNC
        sub     '0'
        ld      b,46h           ; IM 0
        jr      z,S19
        dec     a
        ld      b,56h           ; IM 1
        jr      z,S19
        dec     a
        ld      b,5Eh           ; IM 2
        jr      z,S19
        ld      c,a
        ld      a,(CPU)
        cp      2               ; Z280?
        jp      nz,OERROR
        dec     c
        ld      b,4Eh           ; IM 3
        jp      nz,OERROR
S19:    ld      a,b
        jp      EMITB

;       SUBTTL  Instruction Class 2 - Rotates
;
;       Class 2 - Rotates
;
CL2:    call    GNC
        or      a
        jp      z,OERROR
        cp      '('
        jr      z,S30           ; branch if () form
        call    BACKUP
        call    EVALREG         ; must be a single register
        cp      RNAME
        jp      nz,OERROR
        ld      a,0CBh
        call    EMITB
        ld      a,(OPCODE)
        ld      b,a
        ld      a,(VAL)
        or      b
        jp      EMITB
;
S30:    call    EVALREG
        cp      RPNAME
        jp      nz,OERROR       ; must be rpair
        ld      a,l
        cp      4               ; only HL,IX,IY allowed
        jp      nz,OERROR
        ld      a,h
        or      a
        jp      z,S40           ; branch if (HL)
        call    EMITB           ; do IX and IY similarly
        ld      a,0CBh
        call    EMITB
        call    EVALNEW         ; evaluate index
        call    REQCHR          ; bypass )
        defb    ')'
        ld      a,(EVMODE)
        call    REQ8U           ; ensure 8-bit index (REQ8S?)
        ld      a,l
        call    EMITB
        ld      a,(OPCODE)
        add     a,6
        jp      EMITB
;
S40:    call    REQCHR
        defb    ')'
        ld      a,0CBh
        call    EMITB
        ld      a,(OPCODE)
        add     a,6
        jp      EMITB

;       SUBTTL  Instruction Class 3 - Absolute Jumps & Calls
;
;       Class 3 - Jumps - Calls
;
CL3:    call    GNC
        or      a
        jp      z,OERROR
        cp      '('
        jr      z,S53           ; branch if () form
        cp      '<'
        jp      z,S57           ; branch if <> form
        call    BACKUP
        call    EVALCND         ; try conditional
        cp      CONDD
        jr      nz,S44          ; jump if not conditional
        call    REQCHR
        defb    ','
        ld      a,l
        rlca
        rlca
        rlca
        ld      b,a
        ld      a,(OPCODE)
        and     0C6h
        or      b
        ld      b,a
        call    GNC
        or      a
        jp      z,OERROR
        cp      '('
        jr      z,S56
        cp      '<'
        jp      z,S58
        ld      a,b
        call    EMITB
        call    BACKUP
        call    EVALNEW
        jp      EMITV           ; emit address
;
S44:    ld      a,(OPCODE)
        call    EMITB
        jp      EMITV           ; emit address
;
;       JP      (HL/IX/IY)
;
S53     equ     $               ; process () form
        call    EVALSRG
        cp      RSNAME
        jr      z,S57A          ; branch if special reg (Z280)
        cp      RPNAME          ; else must be rpair
        jp      nz,OERROR
        ld      a,l
        cp      4
        jp      nz,OERROR       ; must be HL,IX,IY
        ld      a,(OPCODE)
        cp      0CDh            ; branch if 'CALL (HL)' (Z280)
        ld      b,a
        jr      z,S54
        ld      a,h
        or      a
        call    nz,EMITB        ; emit prefix if index register
        ld      a,0E9h
        jr      S55
;
;       CALL/JP cond,(HL)       ; Z280
;
S56:    push    bc
        call    EVALSRG
        pop     bc
        cp      RSNAME
        jr      z,S58A          ; branch if special reg
        cp      RPNAME          ; else must be rpair
        jp      nz,OERROR
        ld      a,l
        cp      4
        jp      nz,OERROR
S54:    ld      a,(CPU)
        cp      2
        jp      nz,OERROR       ; Z280 only
        ld      a,h
        or      a
        jp      nz,OERROR       ; IX/IY not allowed
        ld      a,0DDh
        call    EMITB
        ld      a,b
S55:    call    EMITB
        call    REQCHR
        defb    ')'
        ret
;
;       CALL/JP cond,(PC+addr)  ; Z280
;
S57A:   ld      a,(OPCODE)
        ld      b,a
S58A:   push    bc
        call    EVALNEW
        pop     bc
        ld      a,(CPU)
        cp      2
        jp      nz,OERROR       ; Z280 only
        ld      a,0FDh
        call    EMITB
        ld      a,b
        call    EMITB
        call    EMITV           ; emit relative address
        call    REQCHR
        defb    ')'
        ret
;
;       CALL/JP cond,<addr>     ; Z280
;
S57:    ld      a,(OPCODE)
        ld      b,a
S58:    push    bc
        call    EVALNEW
        pop     bc
        ld      a,(CPU)
        cp      2
        jp      nz,OERROR       ; Z280 only
        ld      a,0FDh
        call    EMITB
        ld      a,b
        call    EMITB
        ld      de,0
        call    EMITVR          ; emit relative address
        call    REQCHR
        defb    '>'
        ret

;       SUBTTL  Instruction Class 4 - Relative Jumps
;
;       Class 4 - Relative jumps (JR and DJNZ, also Z280's JAF and JAR)
;
CL4:    ld      a,(OPCODE+1)
        or      a
        jr      nz,S67          ; branch if JAF or JAR
        call    EVALCND         ; try conditional
        cp      CONDD
        jp      nz,S68          ; jump if not conditional
        ld      a,l
        cp      4               ; only Z, NZ, C and NC are legal
        jp      nc,OERROR
        rlca
        rlca
        rlca
        add     a,20h
        call    EMITB
        call    REQCHR
        defb    ','
        call    EVALNEW         ; get target address
        jr      S68A            ; now go process like non-conditional
;
S67:    call    EMITB           ; emit prefix for JAF/JAR
        call    EVALNEW         ; get target address
S68:    ld      a,(OPCODE)
        call    EMITB           ; emit opcode
S68A:   ld      hl,(VAL)
        ld      a,(PCFLAG)      ; fetch PC relative flag
        or      a               ; PC relative value?
        jr      z,S68B          ; skip PC relative
        ex      de,hl
        call    GETPPC          ; get effective PC value
        ex      de,hl
        or      a
        sbc     hl,de           ; subtract effective PC
S68B:   dec     hl
        dec     hl              ; -2
        ld      a,(OPCODE+1)
        or      a
        jr      z,S68C
        dec     hl              ; -3 if JAF/JAR
S68C:   ld      a,l
        call    EMITB
        ld      a,(EVMODE)
        and     SEGMASK+EXTSYM
        ld      c,a
        ld      a,(CURSEG)
        cp      c
        jp      nz,RELERR       ; inter-segment/external not allowed as dest
        ld      a,h             ; else check if within range
        or      a
        jr      z,POSTST
        inc     a
        jr      nz,DERROR
NEGTST: ld      a,l
        or      a
        ret     m
        jr      DERROR
POSTST: ld      a,l
        or      a
        ret     p
DERROR: ld      a,'D'           ; range error
        ld      (ERRFLG),a
        ret

;       SUBTTL  Instruction Class 5 - Restarts
;
;       Class 5 - Restarts
;
CL5:    call    EVALNEW
        ld      a,(VAL)         ; fetch op-code value
        ld      c,a
        and     0C7h            ; mask RST bits out
        jp      nz,OERROR       ; out of value error
        ld      a,c             ; fetch op-code value
        and     38h             ; mask to RST bits
        or      0C7h            ; make RST code
        jp      EMITB

;       SUBTTL  Instruction Class 6 - Arithmetic & Logical
;
;       Class 6 - Arithmetic opcodes
;
;       ADD, SUB, ADC, SBC, AND, OR, XOR, CP
;
CL6:    call    GNC
        or      a
        jp      z,OERROR
        cp      '('
        jp      z,S85           ; branch if () form
        cp      '<'
        jp      z,S85           ; branch if <> form (Z280)
        call    BACKUP
        call    EVALREG
        cp      RNAME           ; single reg
        jr      z,S84
        cp      RPNAME          ; double reg
        jp      z,S96B
;
;       Immediate value - ADD nn
;
S82:    ld      a,(OPCODE)
        add     a,0C6h
        call    EMITB
        ld      hl,(VAL)
        ld      a,(EVMODE)
        call    REQ8U
        ld      a,l
        jp      EMITB
;
;       Single-register name - ADD A,...
;
S84:    call    GNC
        cp      ','             ; check for A,xx form
        jr      nz,S84A
        ld      a,(VAL)
        cp      7
        jp      nz,OERROR       ; first operand can be only A
        call    GNC
        or      a
        jp      z,OERROR
        cp      '('
        jr      z,S85           ; jump if A,(rp)
        cp      '<'
        jp      z,S85           ; jump if A,<nnnn> (Z280)
        call    BACKUP
        call    EVALREG
        cp      RPNAME
        jp      z,OERROR        ; rpair illegal here
        cp      RNAME
        jr      nz,S82          ; process immediate value
S84A:   ld      a,(VAL+1)
        ld      c,a
        or      a               ; check for index register
        jr      z,S84B
        ld      a,(CPU)
        dec     a
        jp      z,OERROR        ; IXH/IXL/IYH/IYL not supported by Z180
        ld      a,c
        call    EMITB           ; emit prefix
S84B:   ld      a,(OPCODE)
        add     a,80h
        ld      b,a
        ld      a,(VAL)         ; get register value
        add     a,b
        jp      EMITB
;
;       ADD     (xx)
;
S85:    call    EVBRKT          ; evaluate expression in ()
        ld      hl,ADTBL
        jp      SWITCH
;
ADTBL:  defw    S92             ; (addr)
        defw    OERROR          ; (r)
        defw    S87             ; (rp)
        defw    S91             ; (rp+d8)
        defw    S87A            ; (rp+d16)  Z280
        defw    S88             ; (x+y)     Z280
        defw    S86             ; (PC+d16)  Z280
        defw    S86             ; <addr>    Z280
;
;       ADD     (addr)
;
S92:    ld      a,(CPU)
        cp      2
        jp      nz,OERROR       ; Z280 only
        ld      c,87h
        jr      S93A
;
;       ADD     (rp)
;
S87:    ld      a,(REGVAL)
        cp      6
        jr      z,S93           ; branch if (SP) (Z280)
        cp      4
        jp      nz,OERROR       ; error if not (HL)
        ld      a,(OPCODE)
        add     a,86h
        jp      EMITB
;
;       ADD     (IX/IY+d8)
;
S91:    ld      a,(REGVAL+1)
        call    EMITB           ; emit index register prefix
        ld      a,(OPCODE)
        add     a,86h
        call    EMITB
        ld      a,(VAL)         ; index type/mode already checked
        jp      EMITB           ; emit index
;
;       ADD     (rp+d16)        ; Z280
;
S87A:   ld      hl,(REGVAL)
        ld      a,l
        cp      6
        jr      z,S93           ; branch if SP (Z280)
        ld      a,h
        or      a
        ld      c,83h           ; HL
        jr      z,S90
        cp      0DDh
        ld      c,81h           ; IX
        jr      z,S90
        inc     c               ; IY
S90:    ld      a,0FDh
        call    EMITB
        ld      a,(OPCODE)
        add     a,c
        call    EMITB
        jp      EMITV           ; emit address
;
;       ADD     (SP+d16)
;
S93:    ld      a,(CPU)
        cp      2               ; Z280 only
        jp      nz,OERROR
        ld      c,80h
S93A:   ld      a,0DDh
        call    EMITB
        ld      a,(OPCODE)
        add     a,c
        call    EMITB
        jp      EMITV           ; emit address
;
;       ADD     (x+y)           ; Z280
;
S88:    ld      a,0DDh
        call    EMITB
        ld      a,(OPCODE)
        add     a,b
        add     a,80h
        jp      EMITB
;
;       ADD     <nnnn>          ; Z280
;       ADD     (PC+nnnn)       ; Z280
;
S86:    ld      a,0FDh
        call    EMITB
        ld      a,(OPCODE)
        add     a,80h
        call    EMITB
        ld      a,c
        cp      6
        jp      z,EMITV         ; emit index if (PC+nnnn)
        ld      de,0
        jp      EMITVR          ; else emit relative address
;
;       Process register pair ADD,SUB,ADC,SBC
;
S96B:   ld      a,l
        cp      4
        jp      nz,OERROR       ; 1st operand can be only HL, IX or IY
        call    REQCHR
        defb    ','             ; check for legal delimiter
        ld      a,(OPCODE+1)
        or      a
        jp      z,OERROR        ; error if AND, OR, XOR, CP, etc.
        ld      a,h
        or      a
        call    nz,EMITB        ; emit prefix if index register
        ld      a,(OPCODE+1)
        cp      9
        jr      z,S96C          ; branch if ADD
        ld      a,h
        or      a
        jr      z,S96D          ; branch if not an index register
        ld      a,(CPU)
        cp      2
        jp      nz,OERROR       ; ADC/SBC IX/IY supported only by Z280
S96D:   ld      a,0EDh          ; prefix for ADC and SBC
        call    EMITB
S96C:   push    hl
        call    EVALREG
        pop     de
        cp      RNAME           ; jump if single register
        jr      z,S96F
        cp      RPNAME          ; ensure register pair
        jp      nz,OERROR
        ld      a,l
        cp      4
        jr      nz,S96E
        ld      a,h
        cp      d               ; can't be 'IX,IY' or 'IY,IX'
        jp      nz,OERROR       ;       or 'IX,HL' or 'IY,HL'
S96E:   ld      a,l
        rlca
        rlca
        rlca
        ld      b,a
        ld      a,(OPCODE+1)
        add     a,b
        jp      EMITB
;
;       ADD     HL/IX/IY,A      ; Z280
;
S96F:   ld      a,(CPU)
        cp      2
        jp      nz,OERROR       ; Z280 only
        ld      a,(VAL)
        cp      7
        jp      nz,OERROR       ; only reg A allowed
        ld      a,(OPCODE+1)
        cp      9
        jp      nz,OERROR       ; error if not ADD
        ld      a,0EDh
        call    EMITB
        ld      a,6Dh
        jp      EMITB

;       SUBTTL  Instruction Class 7 - I/O instructions
;
;       Class 7 - I/O Instructions
;
CL7:    ld      a,(OPCODE)
        ld      hl,IOTBL
        jp      SWITCH
;
IOTBL:  defw    ZIN             ; IN
        defw    ZOUT            ; OUT
        defw    ZIN0            ; IN0   (Z180)
        defw    ZOUT0           ; OUT0  (Z180)
        defw    ZTSTIO          ; TSTIO (Z180)
        defw    ZTSTI           ; TSTI  (Z280)
        defw    ZINW            ; INW   (Z280)
        defw    ZOUTW           ; OUTW  (Z280)
;
;       IN instruction
;
ZIN:    call    GNC
        or      a
        jp      z,OERROR
        cp      '('
        jr      z,S100          ; branch if () form (Z280)
        cp      '<'
        jp      z,S100          ; branch if <> form (Z280)
        call    BACKUP
        call    EVALREG
        cp      RPNAME
        jp      z,S102          ; branch if rpair (Z280)
        cp      RNAME
        jp      nz,OERROR
        push    hl
        call    REQCHR
        defb    ','
        call    GNC
        cp      '('
        jp      nz,OERROR
        call    EVBRKT          ; get second operand
        pop     bc
        or      a
        jr      z,S98           ; branch if (addr)
        dec     a
        jp      nz,OERROR       ; else only reg C allowed
;
;       IN      r,(C)
;
S97:    ld      a,b
        or      a
        jr      z,S97B
        ld      a,(CPU)
        dec     a
        jp      z,OERROR        ; IXH/IXL/IYH/IYL not supported by Z180
        ld      a,b
        call    EMITB           ; emit prefix
S97B:   ld      a,0EDh
        call    EMITB
        ld      a,c
        rlca
        rlca
        rlca
        ld      c,a
        ld      a,(OPCODE+1)
        add     a,c
        jp      EMITB
;
;       IN      A,(nn)
;
S98:    ld      a,c
        cp      7
        jp      nz,OERROR       ; only reg A allowed
        ld      a,0DBh
        call    EMITB
        ld      hl,(VAL)
        ld      a,(EVMODE)
        call    REQ8U
        ld      a,l
        jp      EMITB
;
;       IN      (...),(C)       ; Z280
;
S100:   ld      c,a
        ld      a,(CPU)
        cp      2
        jp      nz,OERROR       ; Z280 only
        ld      a,c
        call    EVBRKT
        push    af
        call    CHKIOC
        pop     af
        ld      hl,IOTBL1
        jp      SWITCH
;
IOTBL1: defw    S103            ; (addr)
        defw    OERROR          ; (r)
        defw    S100A           ; (rp)
        defw    S100A           ; (rp+d8)
        defw    S100A           ; (rp+d16)  Z280
        defw    S100B           ; (x+y)     Z280
        defw    S101            ; (PC+d16)  Z280
        defw    S101            ; <addr>    Z280
;
S100A:  ld      a,(REGVAL)
        cp      6
        jr      z,S104          ; branch if SP
        cp      4
        jp      nz,OERROR       ; else only HL/IX/IY allowed
        jr      S105            ; branch if no match
;
S100B:  ld      a,0DDh
        call    EMITB
        ld      a,0EDh
        call    EMITB
        ld      a,b
        rlca
        rlca
        rlca
        ld      c,a
        ld      a,(OPCODE+1)
        add     a,c
        jp      EMITB
;
S103:   ld      a,0DDh
        call    EMITB
        ld      a,0EDh
        call    EMITB
        ld      a,(OPCODE+1)
        add     a,38h
        call    EMITB
        jp      EMITV           ; emit address
;
;       IN      (SP+nnn),(C)    ; Z280
;
S104:   ld      c,0
        ld      a,0DDh
        jr      S107
;
S105:   ld      a,(REGVAL+1)
        cp      0DDh
        ld      c,08h
        jr      z,S106
        cp      0FDh
        ld      c,10h
        jr      z,S106
        ld      c,18h
S106:   ld      a,0FDh
S107:   call    EMITB
        ld      a,0EDh
        call    EMITB
        ld      a,(OPCODE+1)
        add     a,c
        call    EMITB
        jp      EMITV           ; emit address
;
;       IN      <addr>,(C)      ; Z280
;       IN      (PC+addr),(C)   ; Z280
;
S101:   ld      a,0FDh
        call    EMITB
        ld      a,0EDh
        call    EMITB
        ld      a,(OPCODE+1)
        call    EMITB
        ld      a,c
        cp      6
        jp      z,EMITV         ; emit index if (PC+addr)
        ld      de,0
        jp      EMITVR          ; else emit relative address
;
;       IN      HL,(C)          ; Z280
;
S102:   ld      a,(CPU)
        cp      2
        jp      nz,OERROR       ; Z280 only
        ld      hl,(VAL)
        ld      de,4
        call    CMPHD           ; only HL allowed
        jp      nz,OERROR
        call    CHKIOC
        ld      a,0EDh
        call    EMITB
        ld      a,0B7h
        jp      EMITB
;
;       Test for ',(C)' operand
;
CHKIOC: call    REQCHR
        defb    ','
CHKCC:  call    REQCHR
        defb    '('
        call    REQCHR
        defb    'C'
        call    REQCHR
        defb    ')'
        ret
;
;       OUT instruction
;
ZOUT:   call    GNC
        cp      '('
        jp      nz,OERROR
        call    EVBRKT          ; evaluate expression in brackets
        or      a
        jr      z,S109          ; branch if (addr)
        dec     a
        jp      nz,OERROR       ; else must be (C)
;
;       OUT     (C),r
;
        call    REQCHR
        defb    ','
        call    GNC
        or      a
        jp      z,OERROR
        cp      '('
        jr      z,S108          ; branch if () form (Z280)
        cp      '<'
        jr      z,S108          ; branch if <> form (Z280)
        call    BACKUP
        call    EVALREG
        cp      RPNAME
        jr      z,S109A         ; branch if rpair (Z280)
        cp      RNAME
        jp      nz,OERROR       ; ensure single register
        ld      bc,(VAL)
        jp      S97
;
;       OUT     (C),HL          ; Z280
;
S109A:  ld      a,(CPU)
        cp      2
        jp      nz,OERROR       ; Z280 only
        ld      de,4
        call    CMPHD
        jp      nz,OERROR       ; only HL is valid
        ld      a,0EDh
        call    EMITB
        ld      a,0BFh
        jp      EMITB
;
;       OUT     (nn),A
;
S109:   call    REQCHR
        defb    ','
        call    REQCHR
        defb    'A'
        ld      a,0D3h
        call    EMITB
        ld      hl,(VAL)
        ld      a,(EVMODE)
        call    REQ8U
        ld      a,l
        jp      EMITB
;
;       OUT     (C),(...)       ; Z280
;
S108:   ld      c,a
        ld      a,(CPU)
        cp      2
        jp      nz,OERROR
        ld      a,c
        call    EVBRKT          ; evaluate expression in brackets
        ld      hl,IOTBL1       ; process via common code
        jp      SWITCH
;
;       IN0     r,(p)           ; Z180
;
ZIN0:   call    EVALREG
        cp      RNAME           ; ensure single register
        jp      nz,OERROR
        ld      hl,(VAL)
        ld      a,h
        or      a
        jp      nz,OERROR       ; IXH,IXL,IYH,IYL not permitted
        ld      a,0EDh
        call    EMITB
        ld      a,l
        rlca
        rlca
        rlca
        call    EMITB
        call    REQCHR
        defb    ','
        call    REQCHR
        defb    '('
        call    EVALNEW         ; get port number
        ld      a,l
        call    EMITB
        call    REQCHR
        defb    ')'
        ret
;
;       OUT0    (p),r           ; Z180
;
ZOUT0:  call    REQCHR
        defb    '('
        call    EVALNEW         ; get port number
        push    hl
        call    REQCHR
        defb    ')'
        call    REQCHR
        defb    ','
        call    EVALREG
        cp      RNAME           ; ensure single register
        jp      nz,OERROR
        ld      hl,(VAL)
        ld      a,h
        or      a
        jp      nz,OERROR       ; IXH,IXL,IYH,IYL not permitted
        ld      a,0EDh
        call    EMITB
        ld      a,l
        rlca
        rlca
        rlca
        or      01h
        call    EMITB
        pop     hl
        ld      a,l
        jp      EMITB
;
;       TSTIO   p               ; Z180
;
ZTSTIO: call    EVALNEW         ; get port number
        ld      a,0EDh
        call    EMITB
        ld      a,74h
        call    EMITB
        ld      hl,(VAL)
        ld      a,(EVMODE)
        call    REQ8U
        ld      a,l
        jp      EMITB
;
;       TSTI    (C)             ; Z280
;
ZTSTI:  call    CHKCC           ; test for (C) oerand
        ld      a,0EDh
        call    EMITB
        ld      a,70h
        jp      EMITB
;
;       INW     HL,(C)          ; Z280
;
ZINW:   call    EVALREG
        cp      RPNAME
        jp      nz,OERROR
        jp      S102            ; continue via common IN code
;
;       OUTW    (C),HL
;
ZOUTW:  call    CHKCC           ; syntax requires (C)
        call    REQCHR
        defb    ','
        call    EVALREG
        cp      RPNAME
        jp      nz,OERROR
        jp      S109A           ; continue via common OUT code

