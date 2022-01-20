;**********************************************************************;
;                                                                      ;
;   This file is part of ZSM4, a Z80/Z180/Z280 relocatable macro-      ;
;   assembler written in Z80 assembly.                                 ;
;   Copyright (C) 2017-2020, Hector Peraza.                            ;
;   Modified by Ladislau Szilagyi ( dec 2021 - jan 2022 )              ;                           ;                                                                      ;
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

        global  TYPTBL,CL1,CL2,CL3,CL4,CL5,CL6,CL7,CL8,CL9
        global  CL10,CL11,CL12,CL13,CL14,CL15,CL16,CL17,CL18,CL19
        global  CL20,CL21,CL22,CL23,CL24,CL25,CL26,CL27

        global  SAVVAL,EMITB,REQ8U,PTR1,SWITCH
        global  EMITV,EMITVR,OPCODE

        global  S270

        psect   text

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
S225:   ld      a,(CPU)
        cp      2
        jp      nz,OERROR       ; Z280 only
        call    EVALSRG
        cp      RSNAME
        jr      z,S228          ; branch if special reg
        cp      RNAME
        jp      z,OERROR        ; can't be single reg
        cp      RPNAME
        jr      nz,S226
        ld      de,4
        call    CMPHD           ; only (HL) allowed
        jp      nz,OERROR
        ld      a,0DDh
        call    EMITB
        ld      a,(OPCODE)
        call    EMITB
        call    REQCHR
        defb    ')'
        ret
;
;       PUSH    (addr)          ; Z280
;
S226:   ld      a,0DDh
        call    EMITB
        ld      a,(OPCODE)
        add     a,10h
        call    EMITB
        call    EMITV
        call    REQCHR
        defb    ')'
        ret
;
;       PUSH    <addr>          ; Z280
;
S227:   ld      a,(CPU)
        cp      2
        jp      nz,OERROR
        call    EVALNEW
        ld      a,0DDh
        call    EMITB
        ld      a,(OPCODE)
        add     a,30h
        call    EMITB
        ld      de,0
        call    EMITVR          ; emit relative address
        call    REQCHR
        defb    '>'
        ret
;
;       PUSH    (PC+addr)       ; Z280
;
S228:   ld      a,(CPU)
        cp      2
        jp      nz,OERROR
        ld      a,l
        cp      3
        jp      nz,OERROR       ; only PC allowed
        call    EVALNEW
        ld      a,0DDh
        call    EMITB
        ld      a,(OPCODE)
        add     a,30h
        call    EMITB
        call    EMITV           ; emit address
        call    REQCHR
        defb    ')'
        ret

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
S236:   ld      a,(CPU)
        cp      2
        jp      nz,OERROR       ; Z280 only
        call    REQCHR
        defb    ','
        ld      hl,(VAL)
        ld      a,l
        cp      7
        jr      z,S237          ; branch if A
        cp      4
        jp      nz,OERROR
        ld      a,h
        or      a
        jp      nz,OERROR       ; else only H allowed
        call    EVALREG
        cp      RNAME
        jp      nz,OERROR
        ld      a,l
        cp      5
        jp      nz,OERROR
        ld      a,h
        or      a
        jp      nz,OERROR       ; second operand must be L
        ld      a,0EDh
        call    EMITB
        ld      a,0EFh
        jp      EMITB
;
;       EX      A,...           ; Z280
;
S237:   call    GNC
        or      a
        jp      z,OERROR
        cp      '('
        jr      z,S238          ; branch if () form
        cp      '<'
        jp      z,S238          ; branch if <> form
        call    BACKUP
        call    EVALREG
        cp      RNAME
        jp      nz,OERROR       ; error if not single register
        ld      a,h
        or      a
        call    nz,EMITB
        ld      a,0EDh
        call    EMITB
        ld      a,(VAL)
        rlca
        rlca
        rlca
        add     a,07h
        jp      EMITB
;
;       EX      A,(...)
;
S238:   call    EVBRKT          ; evaluate ()
        ld      hl,EXTBL
        jp      SWITCH
;
EXTBL:  defw    S238M           ; (addr)
        defw    OERROR          ; (r)
        defw    S238A           ; (rp)
        defw    S238B           ; (rp+d8)
        defw    S238E           ; (rp+d16)
        defw    S238D           ; (x+y)
        defw    S239            ; (PC+d16)
        defw    S239            ; <addr>
;
;       EX      A,(addr)        ; Z280
;
S238M:  ld      a,0DDh
        call    EMITB
        ld      a,0EDh
        call    EMITB
        ld      a,3Fh
        call    EMITB
        jp      EMITV           ; emit address
;
;       EX      A,(rp)          ; Z280
;
S238A:  ld      a,(REGVAL)
        cp      6
        jp      z,S238C         ; branch if SP
        cp      4
        jp      nz,OERROR       ; else must be HL/IX/IY
;
;       EX      A,(HL)          ; Z280
;
        ld      a,0EDh          ; else use short form
        call    EMITB
        ld      a,37h
        jp      EMITB
;
;       EX      A,(IX/IY+d8)
;
S238B:  ld      a,(REGVAL+1)
        call    EMITB           ; emit prefix
        ld      a,0EDh
        call    EMITB
        ld      a,37h
        call    EMITB
        ld      a,(VAL)
        jp      EMITB           ; output index (type/mode already checked)
;
;       EX      A,(rp+d16)
;
S238E:  ld      hl,(REGVAL)
        ld      a,l
        cp      6
        jr      z,S238C         ; branch if (SP)
        ld      a,h
        cp      0DDh
        ld      c,08h           ; IX
        jr      z,S238H
        cp      0FDh
        ld      c,10h           ; IY
        jr      z,S238H
        ld      c,18h           ; HL
S238H:  ld      a,0FDh
        call    EMITB
        ld      a,0EDh
        call    EMITB
        ld      a,c
        add     a,07h
        call    EMITB
        jp      EMITV           ; emit address
;
;       EX      A,(SP+d16)      ; Z280
;
S238C:  ld      b,00h
        call    S238D
        jp      EMITV           ; emit address
;
;       EX      A,(x+y)         ; Z280
;
S238D:  ld      a,0DDh
        call    EMITB
        ld      a,0EDh
        call    EMITB
        ld      a,b
        rlca
        rlca
        rlca
        add     a,07h
        jp      EMITB
;
;       EX      A,<addr>        ; Z280
;       EX      A,(PC+addr)     ; Z280
;
S239:   ld      a,0FDh
        call    EMITB
        ld      a,0EDh
        call    EMITB
        ld      a,07h
        call    EMITB
        ld      a,c
        cp      6
        jp      z,EMITV         ; emit address if (PC+addr)
        ld      de,0
        jp      EMITVR          ; else emit relative address

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
;
INCTBL: defw    S295            ; (addr)
        defw    OERROR          ; (r)
        defw    S291            ; (rp)
        defw    S294            ; (rp+d8)
        defw    S297            ; (rp+d16)
        defw    S292            ; (x+y)
        defw    S271            ; (PC+d16)
        defw    S271            ; <addr>
;
;       INC     (addr)          ; Z280
;
S295:   ld      a,(CPU)
        cp      2
        jp      nz,OERROR       ; Z280 only
        ld      c,38h
        jp      S296A
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
S296:   ld      a,(CPU)
        cp      2
        jp      nz,OERROR       ; Z280 only
        ld      c,00h
S296A:  ld      a,0DDh
        call    EMITB
        ld      a,(OPCODE)
        add     a,c
        call    EMITB
        jp      EMITV           ; emit address
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

