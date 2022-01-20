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

        global  EMITB,REQ8U,SWITCH
        global  EMITV,EMITVR,OPCODE,EMITW

        global  S270

        psect   text

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

