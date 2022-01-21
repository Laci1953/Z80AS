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
	GLOBAL	OPCODES
	GLOBAL	OPLEN
	
	GLOBAL	PC
	GLOBAL	REC
	GLOBAL	LEN
	GLOBAL	VAL
	GLOBAL	CPU
	GLOBAL	EFLG
	GLOBAL	PTR1
	GLOBAL	IDADR
	GLOBAL	LFLAG
	GLOBAL	OFLAG
	GLOBAL	IDBUF
	GLOBAL	QFLAG
	GLOBAL	IDLEN
	GLOBAL	UFLAG
	GLOBAL	HOFPG
	GLOBAL	LENDS
	GLOBAL	RADIX
	GLOBAL	UMODE
	GLOBAL	DSPTR
	GLOBAL	MACFLG
	GLOBAL	PCFLAG
	GLOBAL	ENDADR
	GLOBAL	DSFLAG
	GLOBAL	LBLFLG
	GLOBAL	ASEGPC
	GLOBAL	CSEGPC
	GLOBAL	DBWFLG
	GLOBAL	ENDMOD
	GLOBAL	LOCFLG
	GLOBAL	DSEGPC
	GLOBAL	BSEGPC
	GLOBAL	MODIDN
	GLOBAL	MODNAM
	GLOBAL	HDRBUF
	GLOBAL	CLEVEL
	GLOBAL	ERRFLG
	GLOBAL	DEFCPU
	GLOBAL	EQUFLG
	GLOBAL	MAXMEM
	GLOBAL	CURLNE
	GLOBAL	CONDSP
	GLOBAL	IFLIST
	GLOBAL	CURSEG
	GLOBAL	TITLEB
	GLOBAL	SBTTLB
	GLOBAL	BSSIZE
	GLOBAL	ERRCNT
	GLOBAL	PASSNO
	GLOBAL	ZERODS
	GLOBAL	NOLIST
	GLOBAL	COMNTC
	GLOBAL	SYMTBL
	GLOBAL	CNDSTK
	GLOBAL	SYMMOD
	GLOBAL	LSTOPT
	GLOBAL	EVFLGS
	GLOBAL	EVMODE
	GLOBAL	SYMADR
	GLOBAL	OPCODE
	GLOBAL	SYMPTR
	GLOBAL	HOFNAM
	GLOBAL	SAVVAL
	GLOBAL	NEWSYM
	GLOBAL	ENDMARK
	GLOBAL	NAMLEN
	GLOBAL	CMNPTR
	GLOBAL	EXTCHN
	GLOBAL	HOFDAT
	GLOBAL	HOFEND
	GLOBAL	HOFMSG
	GLOBAL	HOFTIM
	GLOBAL	LPFLAG
	GLOBAL	LSTCNT
	GLOBAL	SAVCHN
	GLOBAL	SAVCMN
	GLOBAL	SAVMOD

	psect	text

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
        defb    3+Z280
        defm    'INW'
        defb    6,0,7
        defb    3
        defm    'IRP'
        defb    24,2,27
        defb    3+Z280
        defm    'JAF'
        defb    28h,0DDh,4
        defb    3+Z280
        defm    'JAR'
        defb    20h,0DDh,4
;        defb    3+Z280
;        defm    'LDA'
;        defb    0,0,19
        defb    3
        defm    'LDD'
        defb    0EDh,0A8h,1
        defb    3
        defm    'LDI'
        defb    0EDh,0A0h,1
        defb    3+Z280
        defm    'LDW'
        defb    1,0,8
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
        defb    3+Z180
        defm    'SLP'
        defb    0EDh,76h,1
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
        defb    4
        defm    'ASET'
        defb    1,1,27
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
        defb    4
        defm    'DEFL'
        defb    1,1,27
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
        defb    4+Z280
        defm    'EPUF'
        defb    0EDh,97h,1
        defb    4+Z280
        defm    'EPUI'
        defb    0EDh,9Fh,1
;        defb    4+Z280
;        defm    'EPUM'
;        defb    0EDh,84h,26
        defb    4+Z280
        defm    'EXTS'
        defb    0EDh,64h,23
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
        defb    4+Z280
        defm    'INIW'
        defb    0EDh,82h,1
        defb    4
        defm    'IRPC'
        defb    24,3,27
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
        defb    4+Z180
        defm    'OTDM'
        defb    0EDh,8Bh,1
        defb    4
        defm    'OTDR'
        defb    0EDh,0BBh,1
        defb    4+Z180
        defm    'OTIM'
        defb    0EDh,83h,1
        defb    4
        defm    'OTIR'
        defb    0EDh,0B3h,1
        defb    4+Z180
        defm    'OUT0'
        defb    3,0,7
        defb    4
        defm    'OUTD'
        defb    0EDh,0ABh,1
        defb    4
        defm    'OUTI'
        defb    0EDh,0A3h,1
        defb    4+Z280
        defm    'OUTW'
        defb    7,0,7
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
        defb    4+Z280
        defm    'TSET'
        defb    30h,0,2
        defb    4+Z280
        defm    'TSTI'
        defb    5,0,7
tmp4    equ     $ - OPCOD4
NUMOP4  equ     tmp4/8  ;(4+4)

OPCOD5: 
;	defb    5
;        defm    '.EVEN'
;        defb    20,0,27
        defb    5
        defm    '.LALL'
        defb    11,9,27
        defb    5
        defm    '.LIST'
        defb    11,1,27
        defb    5
        defm    '.SALL'
        defb    11,11,27
        defb    5
        defm    '.XALL'
        defb    11,10,27
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
        defb    5
        defm    'ENTRY'
        defb    17,0,27
        defb    5
        defm    'EXITM'
        defb    26,0,27
        defb    5
        defm    'EXTRN'
        defb    18,0,27
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
        defb    5
        defm    'IFZ80'
        defb    8,11,27
        defb    5+Z280
        defm    'INDRW'
        defb    0EDh,9Ah,1
        defb    5+Z280
        defm    'INIRW'
        defb    0EDh,92h,1
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
        defb    5+Z180
        defm    'OTDMR'
        defb    0EDh,9Bh,1
        defb    5+Z280
        defm    'OTDRW'
        defb    0EDh,9Bh,1
        defb    5+Z180
        defm    'OTIMR'
        defb    0EDh,93h,1
        defb    5+Z280
        defm    'OTIRW'
        defb    0EDh,93h,1
        defb    5+Z280
        defm    'OUTDW'
        defb    0EDh,8Bh,1
        defb    5+Z280
        defm    'OUTIW'
        defb    0EDh,83h,1
        defb    5
        defm    'PSECT'         ;!!!
        defb    33,0,27         ;!!!
        defb    5+Z280          ;!!!
        defm    'RETIL'
        defb    0EDh,55h,1
        defb    5
        defm    'TITLE'
        defb    12,0,27
        defb    5+Z180
        defm    'TSTIO'
        defb    4,0,7
tmp5    equ     $ - OPCOD5
NUMOP5  equ     tmp5/9  ;(5+4)

OPCOD6: 
;	defb    6
;        defm    '.PHASE'
;        defb    29,0,27
        defb    6
        defm    '.RADIX'
        defb    30,0,27
        defb    6
        defm    '.XLIST'
        defb    11,2,27
;        defb    6
;        defm    'COMMON'
;        defb    16,0,27
        defb    6
        defm    'GLOBAL'
        defb    17,0,27
        defb    6
        defm    'IFNDEF'
        defb    8,6,27
        defb    6
        defm    'IFZ180'
        defb    8,12,27
        defb    6
        defm    'IFZ280'
        defb    8,13,27
        defb    6
        defm    'MACLIB'
        defb    23,1,27
;        defb    6+Z280
;        defm    'MULTUW'
;        defb    0C3h,0,15
        defb    6+Z280
        defm    'PCACHE'
        defb    0EDh,65h,1
        defb    6
        defm    'PUBLIC'
        defb    17,0,27
        defb    6
        defm    'SUBTTL'
        defb    12,1,27
tmp6    equ     $ - OPCOD6
NUMOP6  equ     tmp6/10 ;(6+4)

OPCOD7: defb    7
        defm    '.LFCOND'
        defb    11,3,27
        defb    7
        defm    '.PRINTX'
        defb    31,0,27
        defb    7
        defm    '.SFCOND'
        defb    11,4,27
        defb    7
        defm    'INCLUDE'
        defb    23,0,27
tmp7    equ     $ - OPCOD7
NUMOP7  equ     tmp7/11 ;(7+4)

OPCOD8: 
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

        psect   data

;-----------------------------------------------------------------------
;               COMMON DATA AREA
;-----------------------------------------------------------------------

HOFMSG: defb    0Ch
        defm    'ZSM '
        defb    VER1
        defm    '.'
        defb    VER2
        defb    TAB
        defm    'Source file: '
HOFNAM: defm    '         '
        defb    TAB
HOFDAT: defm    '             '
HOFTIM: defm    '        '
        defb    TAB
        defm    'Page '
HOFPG:  defm    '    '
HOFEND: defb    0

HDRBUF: defs    HDRSZ   ; line header buffer
REC:    defs    RECMAX  ; input line, must follow HDRBUF

TITLEB: defs    81      ; title buffer (80 chars + trailing null)
SBTTLB: defs    61      ; subtitle buffer (60 chars + trailing null)

IDLEN:  defs    1       ; length of identifier
IDBUF:  defs    IDMAX   ; current identifier

NAMLEN: defb    6       ; max REL symbol length (5..8)

MODNAM: defs    1       ; length
        defs    8       ; module name
MODIDN: defs    1       ; length
        defs    8       ; module ID

CPU:    defs    1       ; target CPU type: 0=Z80, 1=Z180, 2=Z280
DEFCPU: defs    1       ; default CPU type from command line
PC:     defs    2       ; current program counter
ASEGPC: defs    2       ; current absolute segment counter
CSEGPC: defs    2       ; current code segment counter
DSEGPC: defs    2       ; current data segment counter
BSEGPC: defs    2       ; current bss segment counter
LEN:    defs    1       ; length of current instruction
LENDS:  defs    2       ; for DEFS
CURSEG: defs    1       ; current segment: 40h=CODE, 80h=DATA, C0h=BSS ;COMMON
;CSSIZE:        defs    2       ; code segment size !!!!!!!!!!!!!!!!!!!
;DSSIZE:        defs    2       ; data segment size !!!!!!!!!!!!!!!!!!!
BSSIZE: defs    2       ; BSS segment size
;CURCMN:        defs    2       ; pointer to current COMMON segemt
;LASTCM:        defs    2       ; pointer to last selected COMMON segment
PTR1:   defs    2       ; points to next char in REC
PASSNO: defs    1       ; current pass: 0=pass 1, FF=pass 2
CURLNE: defs    1       ; current line number for paging output
EQUFLG: defs    1       ; if non-zero VAL is used instead of PC for print
LBLFLG: defs    1       ; if non-zero, force PC output to listing
DSFLAG: defs    1       ; if non-zero LENDS is used for print
DBWFLG: defs    1       ; DB/DC/DW flag
LOCFLG: defs    1       ; if non-zero, loc counter is pending output
NEWSYM: defs    1       ; new symbol flag
ENDADR: defs    2       ; expression value on END statement
ENDMOD: defs    1       ; expression result mode on END statement
ENDMARK:defb	0	; 0 if no END start
EFLG:   defs    1       ; end of program flag (to allow printing of END stmt)
OPCODE: defs    2       ; current opcode from symbol table
RADIX:  defs    2       ; default radix for numeric conversion
COMNTC: defs    1       ; .COMMENT delimiter char

VAL:    defs    2       ; return from EVAL routine      !   do   !
EVMODE: defs    1       ; expression result mode        !  not   !
EXTCHN: defs    2       ; External chain address        ! change !
CMNPTR: defs    2       ; pointer to COMMON segment     ! order  !

SAVVAL: defs    2       ; saved contents of VAL         !   do   !
SAVMOD: defs    1       ; saved contents of EVMODE      !  not   !
SAVCHN: defs    2       ; saved contents of EXTCHN      ! change !
SAVCMN: defs    2       ; saved contents of CMNPTR      ! order  !

LPFLAG: defs    1       ; listing line pending flag
LSTCNT: defs    1       ; character count of object code field in listing
LFLAG:  defs    1       ; listing flag:'A-D' = PRN file destn drive
                        ; 'Z' = no listing; 'X' = listing to screen
                        ; 'Y' = listing to screen, errors echoed to printer
                        ; 'P' = listing to printer
OFLAG:  defs    1       ; object flag: 'Z' = no obj, 'A-D' = REL file destn drive
QFLAG:  defs    1       ; quiet flag
UMODE:  defs    1       ; if set, treat all undefined symbols as externals
ZERODS: defs    1       ; if set, initialize DEFS memory to zeros
ERRFLG: defs    1       ; error character for this line
ERRCNT: defs    2       ; error count
MACFLG: defs    1       ; MACRO expansion flag for listing
SYMPTR: defs    2       ; address of next symbol table entry
MAXMEM: defs    2       ; maximum usable memory address
DSPTR:  defs    2       ; pointer to start of dynamic storage
PCFLAG: defs    1       ; PC relative value in EVAL
UFLAG:  defs    1       ; undefined flag from EVAL, 0 = all ok, 1 or >1 = undefined
EVFLGS: defs    1       ; flag field from last SYMLUK
LSTOPT: defs    1       ; list options
IFLIST: defs    1       ; set true to suppress listing on IF, ELSE, ENDIF
                        ;  when "LIST NOCOND" is current.
NOLIST: defs    1       ; set to true to avoid listing TITLE, FORM, PAGE, EJECT
;PHFLAG: defs    1       ; .PHASE flag
;PHDIFF: defs    2       ; .PHASE offset

CONDSP: defw    CNDSTK  ; conditionals stack pointer
        defs    CSTKSZ  ; conditionals stack
CNDSTK: defb    0FFh    ; we always start true
CLEVEL: defb    0       ; conditionals stack level

SYMMOD: defs    1       ; symbol address mode
SYMADR: defs    2       ; address of data field for last SYMENT
IDADR:  defs    2       ; address of ID field for last SYMENT

;-----------------------------------------------------------------------
;
;       Each symbol table entry is of varying length.
;
;       The first byte contains the length in the lower 4 bits
;       and flags in the upper 4 bits.
;
;       This limits the max length of an identifier to 16 bytes.
;
;       Following the flag/length byte is the name which may be from
;       1 to IDMAX bytes in length.
;
;       Following the name are 2 bytes of value (lo,hi), 1 byte
;       of address mode and 2 extra value bytes (used. e.g by
;       COMMON variables).
;
;       The table is scanned sequentially and is ended by a 00 byte.
;
;-----------------------------------------------------------------------

SYMTBL: defs    2       ; address of first available sym table slot

;       END

