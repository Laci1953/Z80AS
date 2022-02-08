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

        global  TYPTBL,CL1,CL2,CL3,CL4,CL5,CL6,CL7,CL8,CL9
        global  CL10,CL11,CL12,CL13,CL14,CL15,CL16,CL17,CL18,CL19
        global  CL20,CL21,CL22,CL23,CL24,CL25,CL26,CL27

        global  EVFLGS,SAVVAL,EMITB,REQ8U,PTR1,ID,IDLEN,IDBUF,SWITCH
        global  EMITV,EMITVR,SAVVARS,EMITSV,OPCODE,CHK8U,VALERR,EMITSR
	global	UCASE

        psect   text

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
S128    equ     $               ; process rname
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
;
LDTBL1: defw    S134            ; (addr)
        defw    OERROR          ; (r)
        defw    S135            ; (rp)
        defw    S140            ; (rp+d8)
        defw    S136            ; (rp+d16)  Z280
        defw    S144            ; (x+y)     Z280
        defw    S150            ; (PC+d16)  Z280
        defw    S150            ; <addr>    Z280
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
;
LDTBL2: defw    S160            ; (addr)
        defw    OERROR          ; (r)
        defw    S161            ; (rp)      Z280
        defw    S162            ; (rp+d8)   Z280
        defw    S163            ; (rp+d16)  Z280
        defw    S164            ; (x+y)     Z280
        defw    S166            ; (PC+d16)  Z280
        defw    S166            ; <addr>    Z280
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
;
LDTBL3: defw    S187            ; (addr)
        defw    OERROR          ; (r)
        defw    S200            ; (rp)
        defw    S210            ; (rp+d8)
        defw    S202            ; (rp+d16)  Z280
        defw    S197            ; (x+y)     Z280
        defw    S219            ; (PC+d16)  Z280
        defw    S219            ; <addr>    Z280
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

