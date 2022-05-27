;**********************************************************************;
;                                                                      ;
;   This file is part of ZSM4, a Z80/Z180/Z280 relocatable macro-      ;
;   assembler written in Z80 assembly.                                 ;
;   Copyright (C) 2017-2021, Hector Peraza.                            ;
;                                                                      ;
;   This work is derived from Z80ASM, originally written by Michael    ;
;   G. Lehman (1977) and with modifications by Ray Halls (1992) and    ;
;   Neil Harrison (1983). See history of changes below.                ;
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

;-----------------------------------------------------------------------
;
;	Z80ASM - Z80 RESIDENT ASSEMBLER
;
;	Originally written in 8080 assembly language for assembly
;	by the CP/M assembler. Now in Zilog mnemonics so that it can
;	assemble itself!
;
;-----------------------------------------------------------------------
;
;	Original - Michael G. Lehman, LEHMAN CONSULTING SERVICES
;	(C) COPYRIGHT 1977
;
;	US User Group, Vol 16
;
;-----------------------------------------------------------------------
;
;	Modified by Ray Halls
;	August 1982
;	for CP/M Users Group (UK)
;
;	   i) Correction to OP-CODE handling
;	  ii) Addition of Console Output and Print File for Listing pass
;	 iii) Additional error reporting
;	  iv) Expansion of DEFB facilities
;	   v) More comprehensive Relative displacement computation
;	  vi) Accurate assembly of Labels
;	 vii) Unsigned 16-bit by 8-bit divide added to expression handling
;
;	October 1982
;
;	Bugs in NEG and I/O Opcode Routines fixed.
;
;-----------------------------------------------------------------------
;
;	Modified even more by Neil Harrison
;	January 1983
;
;	   i) added DB,DW,DS as equivalent to DEFB,DEFW,DEFS
;	  ii) permitted full CP/M 2.x drive specification A: to P:,
;	      except for list option A: to O:, P being used to direct
;	      listing to printer
;	 iii) forced errors & error count to always display on console
;	  iv) Made divide into true 16 bit by 16 bit operation
;	   v) Added "modulo" function using '%'
;	  vi) Added IF, ELSE, ENDIF conditional pseudo ops
;	 vii) Increased HEX file record size to 24 bytes to reduce
;	      disk usage.
;	viii) Added option to convert spaces to tabs in PRN and printer
;	      output. This greatly reduces PRN file size.
;	  ix) Added symbol table overflow check
;	   x) Added conditional assembly option for Z80 processor.
;	      Principally used in place of MOVE subroutine. Where
;	      convenient MOVE replaced by LDIR, an 8080 subroutine
;	      which has the same register usage as the Z80 instruction.
;	  xi) Added sorted symbol table
;	 xii) Added LIST pseudo-op
;	xiii) Added TITLE pseudo-op
;	 xiv) Moved all one time initialisation code into the input
;	      buffer to save space.
;	  xv) Expanded expression evaluation to include: .NOT.,.MOD.,
;	      .SHR.,.SHL.,.EQ.,.NE.,.GT.,.GE.,.LT.,.LE.,.LOW.,.HIGH.
;	      "." is no longer valid in labels! Use "_" instead, e.g. LAB_01
;	 xvi) Limited symbol table searches to avoid confusion with opcodes.
;	xvii) Fixed a bug in the end of file code.
;
;-----------------------------------------------------------------------
;
;	Modified even even more by Hector Peraza
;	September 2017 (on ZSM's 40th anniversary!)
;
;	   i) Fixed many bugs that used to crash the assembler when
;	      it encountered invalid syntax.
;	  ii) More strict syntax checking, but not perfect yet.
;	 iii) Optimized code, removed redundancies. Faster symbol
;	      table searches. The main assembly routine is no longer
;	      a monolithic piece of code with thousands of jumps around,
;	      but was restructured into a set of routines that are
;	      called from, and return to, the main thread. This not
;	      only saves a lot of bytes, but makes adding further
;	      features and enhancements much easier.
;	  iv) Source was split into different modules for easier port
;	      to other Z80 OSes.
;	   v) Abandoned 8080 executable compatibility in favor of the more
;	      efficient Z80 instructions.
;	  vi) Fixed processing of character expressions (e.g. statements
;	      like DW '''a' now work correctly).
;	 vii) Where single-quotes were allowed, double-quotes are now
;	      also allowed, e.g. DEFB "string".
;	viii) Labels are allowed to start with '$'.
;	  ix) '?' and '@' also allowed in labels.
;	   x) The assembler now produces Microsoft REL object files
;             instead of Intel HEX files.
;         xi) 'Expanded' operators are no longer delimited by '.', but
;	      by a space or TAB instead. This was done in order to allow
;	      dots in label names and pseudo-operators, and for compatibility
;	      with M80. Interestingly, this resulted in more compact code.
;	 xii) The expression evaluation routine is now smarter: symbol
;	      names, register names, opcodes and conditional codes no
;	      longer conflict with each other.
;	xiii) Added REL-related pseudo-operators: ASEG, CSEG, DSEG, PUBLIC,
;             GLOBAL, ENTRY, EXT and EXTRN (note that COMMON segments are
;	      not yet supported).
;	 xiv) Added support for the extra Z180 instructions. New .Z80 and
;	      .Z180 pseudo-operators allow selecting target processor type.
;	  xv) Replaced the simple but *very*slow* add-in-a-loop multiply
;	      routine by a faster one. Same for the division routine.
;	 xvi) Spaces and/or TABs are now allowed before/after commas,
;	      brackets, etc. Originally, the assembler would happily
;	      generate wrong object code in cases such as 'ld a,( hl )'
;	      without complain
;	xvii) Listing output format is now more M80-like, with error
;	      code on first column. Since the source line section now
;	      starts at a tab boundary, TAB expansion and compression is
;	      no longer needed. The TABS and NOTABS LIST options were
;	      therefore removed.
;      xviii) Long error messages are now a compile-time option.
;
;-----------------------------------------------------------------------
;
;	More (rather heavy) modifications by Hector Peraza
;	October 2017 - November 2018
;
;	   i) Added .Z280 pseudo-operator, and support for the complete
;	      set of Z280 instructions. Z80 'undocumented' instructions
;	      that use half-index registers are now supported in .Z80
;	      mode.
;	  ii) Added support for COMMON segments.
;	 iii) Added a new pseudo-operator '.EVEN' to align code and/or
;	      data to word boundary (may become useful when writing
;	      programs for the Z280 CPU).
;	  iv) All generated code bytes are now shown on the assembly
;	      listing, instead of just the first 4 bytes.
;	   v) Added DC pseudo-operator that sets the hi-bit of the last
;	      character of a string.
;	  vi) Public labels can now be defined using double-colons '::',
;	      and external references with '##', again for compatibility
;	      with M80.
;	 vii) Expression evaluation now honors operator precedence and
;	      relocation rules. In addition, brackets are now allowed,
;	      e.g. 'LD A,(IX+2*(k+8))', etc.
;	viii) Added support for INCLUDE files. Can be nested.
;	  ix) Opcode table restructured: entries are now grouped by
;	      number of characters and sorted alphabetically in order
;	      to allow for binary searches.
;	   x) Free-format is now allowed (i.e. labels do not have to
;	      start at column 1, and instructions do not have to be
;	      preceded by a blank).
;	  xi) Added support for MACROs, including REPT, IRP and IRPC.
;	 xii) Default source file extension is now MAC.
;	xiii) Added more flavors of IF conditionals: IFT, IFF, IF1,
;	      IF2, IFDEF, IFNDEF, IFB, IFNB, IFIDN, IFDIF, IFZ80, IFZ180,
;	      IFZ280.
;	 xiv) Added new LIST options: MACROS (list all lines of MACRO
;	      expansions), XMACROS (list only the lines that generate
;	      object code), NOMACROS (don't list MACRO expansions).
;	      The default is LIST XMACROS.
;	  xv) Added more pseudo-operators: ASET, DEFL (same as ASET),
;	      DEFM (same as DEFB), DEFC (same as DC), DEFZ, .PHASE,
;	      .DEPHASE, .RADIX, .PRINTX, .COMMENT, .REQUEST, .ODD,
;	      RQST, SUBTTL, NAME, IDENT, .LIST (for compatibility with
;	      M80, same as LIST ON), .XLIST (same as LIST OFF), .LALL
;	      (same as LIST MACROS), .SALL (same as LIST NOMACROS), .XALL
;	      (same as LIST XMACROS), .SFCOND (same as LIST NOCOND) and
;	      .LFCOND (same as LIST COND).
;	 xvi) Added LESS relational operator from ZSM 2.9.
;	xvii) Added NUL operator.
;      xviii) Date and time of assembly is now output to the listing,
;	      if the functionality is supported by the OS.
;	 xix) The PAGE pseudo-operator can be optionally followed by
;	      an expression that sets the page size.
;         xx) Changed the command line syntax. When called without a  
;             command, the assembler enters an interactive command mode 
;             where several files can be assembled without having to 
;             exit to CP/M.
;
;-----------------------------------------------------------------------
;	Modified by Ladislau Szilagyi
;	december 2021 - january 2022
;
;	i)	Command syntax & pseudo ops syntax compatible with ZAS (HiTech's Z80 assembler)
;	ii) 	Symbols up to 31 chars, case sensitive
;	iii)	Object code compatible with HiTech's LINK linker
;
;-----------------------------------------------------------------------

*include        ZSM.INC

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

        psect   text
;
;       Init fields
;
Z80ASM: ld      hl,(MAXMEM)
;        ld      (hl),0          ; init dynamic storage
;        ld      (DSPTR),hl
        ld      hl,(SYMTBL)
        ld      (SYMPT),hl
        call    RESETP          ; reset variables
        ld      a,DEFLNP
        ld      (MAXLNE),a      ; set default lines per page
        ld      (CURLNE),a      ; set up for head of form on first print
        call    INIOBJ
        ld      hl,HOFPG
        ld      (hl),' '
        ld      e,l
        ld      d,h
        inc     de
        ld      bc,3
        ldir                    ; clear page number
        ;continue below

       SUBTTL  Main Assembler module
;-----------------------------------------------------------------------
;
;       MAIN ASSEMBLER MODULE
;
;-----------------------------------------------------------------------
;
;       Main Loop - Read a source record
;                   Process label and opcode
;                   Print line (unless option=N)
;                   Output object code (if necessary)
;                   Back to main loop for next record
;
NEXT:   xor     a
        ld      (UFLAG),a       ; undefined flag
        ld      (LEN),a         ; instruction length
        ld      (EFLG),a
        ld      (EQUFLG),a
        ld      (LBLFLG),a
        ld      (DSFLAG),a
        ld      (DBWFLG),a
        ld      (IFLIST),a
        ld      (NOLIST),a
        ld      h,a
        ld      l,a
        ld      (LENDS),hl      ; init length & flags
        ld      (ENDADR),hl
        ld      (ENDMOD),a
        ld      a,' '
        ld      (ERRFLG),a      ; for this record
        ld      a,(PASSNO)
        or      a
        call    nz,LSTINI       ; init listing
;
;       Process statememt
;
        call    ASSEMB          ; read and process record
;
;       End of statement processing
;
        ld      hl,(CONDSP)     ; point to conditional stack
        ld      a,(hl)          ; get current state
        rra                     ;  into carry
        jr      nc,S401A        ; don't check terminator if false
        call    GNC             ; check valid terminators
        or      a
        jr      z,S401

        ld      a,(ERRFLG)
        cp      ' '
        jr      nz,S401         ; error already, leave it
        ld      a,'O'
        ld      (ERRFLG),a
        jr      S401

S401A:  ld      hl,ERRFLG
        ld      a,(hl)
        rla                     ; forced error?
        jr      c,S401          ; don't change it
        ld      (hl),' '        ; else clear it

S401:   ld      a,(PASSNO)
        or      a
        call    nz,LSTOUT       ; output listing on pass 2

        ld      hl,(PC)
        ld      a,(LEN)
        ld      c,a
        ld      b,0
        add     hl,bc
        ld      de,(LENDS)
        add     hl,de
        ld      (PC),hl
        ld      a,(PASSNO)
        ld      e,a
        ld      a,(DSFLAG)      ; DEFS,DS and pass 2?
        and     e
        jr      z,S402          ; branch if not
	call	SAVEPC
        ld      a,(CURSEG)
	and     SEGMASK
        ld      e,a
        call    WLOC            ; else update current loc
;        jr      S402
;S403:   xor     a
;        call    WOBJ            ; write zero bytes to initialize block
;        dec     hl
;        ld      a,h
;        or      l
;        jr      nz,S403
S402:   ld      a,(EFLG)
        or      a
        jp      z,NEXT          ; go process next record
;
;       Pass 2
;
	call	CheckPSECTS
	jr	c,onlyone
	xor	a		;if more than one from ASEG,CSEG,CUST1,CUST2,CUST3		
	ld	(JFLAG),a	;was used, disable -J option
onlyone:
	ld	a,(JFLAG)	
	or	a		
	jr	z,nojoption	
	ld	a,(PASSNO)	;if J option active, we need an extra pass
	cp	0FFH		
	jp	z,ENDIT		
	ld	a,(JPASS)	
	cpl			
	ld	(JPASS),a	
	or	a		
	jr	nz,keeppass0	
nojoption:			
        ld      a,(PASSNO)
        cpl
        ld      (PASSNO),a
        or      a
        jp      z,ENDIT         ; exit if finished
keeppass0:			
        call    REWIND          ; rewind input file
        xor     a
        ld      (TITLEB),a      ; clean out title
        ld      (SBTTLB),a      ;  and subtitle buffers
        ld      a,DEFLNP
        ld      (MAXLNE),a      ; force header at beginning of listing
        ld      (CURLNE),a
                                ;convert undefined symbols to external
        ld      de,(SYMTBL)
S418:   ld      a,(de)          ; get length
        or      a               ; end of table?
        jr      z,S419          ; exit loop if yes
        and     1FH             ;0Fh
        ld      l,a
        ld      h,0
        inc     de              ;DE=name pointer
        add     hl,de
        inc     hl              ;skip value
        inc     hl              ; HL = address mode field
        ld      a,(hl)
        ld      c,a
        and     UNDEF           ; undefined symbol?
        jr      z,S418C         ; branch if not
        ld      a,c
        and     SEGMASK         ; keep mode (text,data or bss)
        or      EXTSYM          ; and convert symbol to External
        ld      (hl),a
S418C:  inc     hl              ;HL=pointer of next symbol(or EOL)
        ex      de,hl
        jr      S418
S419:
        call    RESETP          ; reset variables
        jp      NEXT            ; process all records again
	
;
;       End of assembly
;
ENDIT:
        ld      de,(SYMTBL)
S420:   ld      a,(de)          ; get length
        or      a               ; end of table?
        jr      z,S421          ; exit loop if yes
        and     1FH             ;0Fh
        ld      b,a             ;B=name length
        ld      l,a
        ld      h,0
        inc     de              ;DE=name pointer
        push    de              ;on stack
        add     hl,de           ;HL= value pointer
        ld      e,(hl)
        inc     hl
        ld      d,(hl)          ; DE = val
        inc     hl              ; HL = address mode field
        ld      c,(hl)          ;C=seg type & mode
        inc     hl              ;HL=pointer of next symbol(or EOL)
        ex      (sp),hl         ;HL=name pointer, pointer of next symbol(or EOL) on stack
                                ;DE=val, B=name length, C=seg&mode
	ld	a,(XFLAG)
	or	a		;if only global/extern symbols must be stored in obj file
	jr	z,S420S
	ld	a,c		;then verify mode
	and	EXTSYM + GBLSYM
	jr	z,S420NS	;and skip writing symbol if symbol is not global or extern
S420S:
        call    WDFENT          ;write symbol to OBJ
S420NS:
        pop     de              ;DE=pointer of next symbol(or EOL)
        jr      S420

S421:
        call    CLSOBJ          ; output entry point record and close obj file

        ld      hl,ERMSG        ; ptr to message
        ld      de,HDRBUF
        ld      bc,ERMSGL       ; message length
        ldir                    ; copy message
        ld      hl,10
        ld      (RADIX),hl      ; reset base to 10
        ld      hl,(ERRCNT)
        push    hl
        call    CVTNUM
        pop     hl
        xor     a
        ld      (de),a
        ld      a,h
        or      l
        jr      nz,S422
        ld      a,(QFLAG)
        or      a
        jr      nz,S423
S422:   ld      a,1             ; set errors flag to force printing
        ld      (ERRFLG),a
S423:   call    PLINE           ; print errors
        ld      hl,OPTMSG        ; ptr to message
        ld      de,HDRBUF
        ld      bc,OPTMSGL       ; message length
        ldir                    ; copy message
        ld      hl,(JCOUNT)
        push    hl
        call    CVTNUM
        pop     hl
        xor     a
        ld      (de),a
        ld      a,h
        or      l
	jr	z,S424
	ld	a,(QFLAG)
	or	a
	jr	nz,S424
	call	PLINE
S424:
        ld      a,' '           ; clear error flag
        ld      (ERRFLG),a

        ld      a,(LFLAG)       ; do we need to print any symbols?
        cp      'Z'
        jp      z,ENDIT4        ; don't print if user asked us not to

        ld      a,(LSTOPT)
        bit     SYMBLS,a        ; are symbols to be generated?
        jp      z,ENDIT4        ; skip if not

        xor     a
        ld      (TITLEB),a      ; clean out title
        ld      (SBTTLB),a      ;  and subtitle buffers
        ld      a,(MAXLNE)
        inc     a               ; force new page
        ld      (CURLNE),a

        ld      c,STMDEF
        call    FNDREC          ; were any macros in the program?
        call    nc,MACROS       ; do it if so

        ld      hl,(SYMTBL)
        ld      de,(SYMPTR)
        call    CMPHD           ; were any symbols in the program?
        call    nz,SYMBOL       ; do it if so

ENDIT4: ld      a,(LFLAG)
        cp      'O'             ; file asked for?
        call    c,CLOSE1        ; yes, close it
        call    CLSINP          ; close input file
	ld	hl,(ERRCNT)
	ld	a,l
	or	h
	ret	nz
	ld	(80H),hl	; mark Z80AS success
        ret                     ; exit

	psect	data

ERMSG:  defb    CR,LF
        defm    'Errors: '
ERMSGL  equ     $-ERMSG
OPTMSG:  defb    CR,LF
        defm    'Jump optimizations done: '
OPTMSGL  equ     $-OPTMSG

	psect	text
;
;       RESETP - Reset variables in preparation for next pass
;
RESETP: xor     a
        ld      (COMNTC),a
        ld      (CLEVEL),a      ; reset conditionals stack
        ld      hl,CNDSTK
        ld      (CONDSP),hl
	ld	(TempCnt),a	; init temporary symbols table
        dec     a
        ld      (hl),a          ; we always start true
        ld      (LOCFLG),a      ; set loc counter pending flag (L80 quirk fix)
	and	0F7H		
        ld      (LSTOPT),a      ; set all list option bits, except sorting symbols
        ld      hl,10
        ld      (RADIX),hl      ; reset radix
        ld      hl,0
        ld      (PC),hl         ; reset PC
        ld      (ASEGPC),hl     ;  for all segments
        ld      (CSEGPC),hl
        ld      (DSEGPC),hl
        ld      (BSEGPC),hl
	ld	(CUST1SEGPC),hl
	ld	(CUST2SEGPC),hl
	ld	(CUST3SEGPC),hl
        ld      (ERRCNT),hl
        ld      (LCLNUM),hl
        ld      a,(DEFCPU)
        ld      (CPU),a         ; reset CPU type
        ld      a,40h           ; CSEG
        ld      (CURSEG),a      ; reset current segment
        ret
;
;       GNR - Get next record - Fill REC until LF or EOF is found
;       Truncate source line if longer than RECMAX-1 characters
;
GNR:    ld      hl,REC
        ld      (PTR1),hl       ; reset pointer

        ld      a,'+'
        ld      (MACFLG),a

        ld      a,(MACLVL)
        or      a               ; reading from macro?
        jp      nz,GNRMAC       ; switch to GNRMAC if yes

        ld      a,' '
        ld      (MACFLG),a

        ld      c,RECMAX-2      ; load buffer count
GNR1:   call    GNB
        cp      EOF
        jr      z,GNR3          ; return if EOF marker is found
        cp      CR
        jr      z,GNR1          ; skip CR
        cp      LF
        jr      z,GNR2          ; LF terminates line
        inc     c
        dec     c
        jr      z,GNR1          ; if buffer full, truncate line
        ld      (hl),a
        inc     hl
        dec     c               ; decr buffer count
        jr      GNR1
GNR3:   push    hl
        push    bc
        call    CLSLIB
        pop     bc
        pop     hl
        ld      a,EOF
        jr      z,GNR2          ; if at root level, return EOF
        ld      a,c
        cp      RECMAX-2
        jr      z,GNR
GNR2:   ld      (hl),0          ; terminate buffer
        ret
;
;       GNC - Get next non-blank character
;       Use PTR1 to index into REC, skip blanks
;
GNC:    push    hl
        ld      hl,(PTR1)
GNC1:   ld      a,(hl)          ; get character
        or      a
        jr      z,GNC2          ; return if end of line
        cp      ';'
        jr      z,GNC3
        inc     hl
        cp      ' '
        jr      z,GNC1          ; skip blanks
        cp      TAB
        jr      z,GNC1
;       call    UCASE           ; do NOT convert L/C to U/C
GNC2:   ld      (PTR1),hl
        pop     hl
        ret
GNC3:   xor     a
        jr      GNC2            ; treat begin of comment as end of line
;
;       BACKUP - Backup PTR1
;
BACKUP: push    hl
        ld      hl,(PTR1)
        dec     hl
        ld      (PTR1),hl
        pop     hl
        ret
;
;       Read next record and process it
;
ASSEMB: call    GNR             ; get next record
        cp      EOF
        jp      z,S321          ; branch if EOF

        ld      a,(COMNTC)
        or      a               ; in .COMMENT block?
        jp      nz,S662

        ld      a,(MDFLVL)
        or      a               ; defining a MACRO?
        jp      nz,MSTORE

        call    GNC             ; skip blanks, get first char
        or      a
        ret     z               ; ignore null line
        ld      hl,0
        ld      (IDADR),hl
        call    BACKUP
	call	ID_OR_NUMBER
;       call    ID              ; get identifier
        cp      ':'
        jr      z,S7            ; branch if label

        ; an identifier found that is not followed by a colon:
        ; may be label, macro or opcode - can get a bit tricky

        cp      ' '             ; test op-code separator
        jr      z,S6
        cp      TAB
        jr      z,S6
        or      a
        jp      nz,NERROR
S6:     ld      a,(IDLEN)
        or      a
        jp      z,NERROR        ; error exit
        call    ID              ; get next identifier, if any
        call    FNDOPC          ; is it an opcode?
        push    af
        push    hl
        ld      a,' '
        ld      (ERRFLG),a      ; clear possible 'L' error
        ld      a,(IDLEN)
        ld      c,a             ; remember if 2nd identifier present
        ld      hl,REC
        ld      (PTR1),hl       ; reset text pointer to start of record
        call    ID              ; and rescan first identifier
        pop     hl
        pop     af
        jr      c,S6A           ; branch if no 2nd ident or not an opcode
        ld      e,(hl)          ; get opcode value
        inc     hl
        ld      d,(hl)
        inc     hl
        ld      a,(hl)
        cp      27              ; pseudo-op?
        jr      nz,S6A          ; branch if not
        ld      a,e
        cp      1               ; EQU/DEFL?
        jr      z,S6B           ; branch if yes
        cp      24              ; MACRO/REPT/IRP/IRPC?
        jr      nz,S6A          ; branch if not
        ld      a,d
        or      a               ; MACRO?
        jr      z,S7A           ; branch if yes - MACRO defs have priority
S6A:    push    bc
        call    FNDMAC          ; lookup macro name
        pop     bc
        jr      c,S6C           ; branch if not found
        ld      bc,(CONDSP)     ; point to conditional stack
        ld      a,(bc)          ; get current state
        rra                     ;  into carry
        jp      c,EXPMAC        ; if true, expand macro (switch context)
        ret                     ; else just return

S6C:    push    bc
        call    FNDOPC          ; lookup opcode
        pop     bc
        jp      nc,S16          ; branch if found
        ld      a,c
        or      a
        jp      z,OERROR
        jr      S7B             ; else process as label ONLY IF 2nd ID present

S6B:    ld      (EQUFLG),a
        jr      S7A
;
;       Process label
;
S7:     call    GNC             ; skip over label delimiter
S7B:    ld      a,1
        ld      (LBLFLG),a      ; set Label flag for listing
S7A:    xor     a
        ld      (SYMMOD),a      ; clear mode bits
        ld      hl,(PTR1)
        ld      a,(hl)
        cp      ':'             ; double-colon means global label
        jr      nz,S8
        call    GNC             ; skip over separator
        ld      a,GBLSYM
        ld      (SYMMOD),a      ; set mode to PUBLIC
S8:     ld      hl,(CONDSP)     ; get conditional state
        ld      a,(hl)          ; get state
        rra
        jp      nc,S12          ; don't enter symbol if false
        ld      a,(ERRFLG)      ; any error
        cp      ' '
        ret     nz              ; exit
        call    GETPPC          ; get effective PC value
        ld      (VAL),hl
        ld      hl,SYMMOD
        ld      a,(CURSEG)
        or      UNDEF           ; set Undefined bit
        or      (hl)            ; add current segment type to mode bits
        ld      (hl),a
        call    ADDSYM          ; enter symbol
        ret     c               ; on error return
        jp      nz,S11          ; jump if new symbol

        ld      a,(EVFLGS)      ; symbol exists, fetch identity char
        ld      c,a
        ld      a,(EQUFLG)      ; EQU or DEFL statement?
        or      a
        jp      nz,S12          ; skip checks if yes (done in EQU code)

        ld      a,(PASSNO)
        or      a
        jr      z,S9            ; branch if pass 1

        ld      a,c             ; pass 2, check type
        cp      MULTDEF         ; multiple defined?
        jr      z,S10           ; multi defined error if yes

        ld      a,(SYMMOD)
        ld      (EVMODE),a
        call    GETPPC          ; get effective PC value
        ld      (VAL),hl
        call    CMPSYM          ; same?
	jp	nc,S12
	ld	a,(JFLAG)	; if JP optimization active
	or	a
	jr	z,PHER
	ld	hl,(VAL)	; just store PC as symbol value
	ex	de,hl
	ld	hl,(SYMADR)
	ld	(hl),e
	inc	hl
	ld	(hl),d
	jr	S12
PHER:   call    PERROR          ; phase error if not
        jr      S12

S9:     inc     hl              ; pass 1, check mode bits
        inc     hl
        ld      a,(hl)
        and     UNDEF           ; undefined?
        dec     hl
        dec     hl
        jr      nz,S9A          ; branch if yes to set value

        ld      a,(SYMMOD)
        ld      (EVMODE),a
        call    GETPPC          ; get effective PC value
        ld      (VAL),hl
        call    CMPSYM          ; same?
        call    c,SETMDF        ; else set multi defined flag
        jr      S12

S9A:    ex      de,hl
        call    GETPPC
        ex      de,hl
        ld      (hl),e          ; set value
        inc     hl
        ld      (hl),d
        inc     hl
        ld      a,(CURSEG)
        ld      c,a
        ld      a,(hl)
        and     20h             ; keep old GLOBAL bit
        or      c
        ld      c,a
        ld      a,(SYMMOD)
        and     20h             ; merge with new GLOBAL bit, if set
        or      c
        ld      (hl),a          ; set new address mode
        jr      S11

S10:    call    MERROR          ; else multi defined error
        jr      S12
;
SETMDF: push    hl
        ld      hl,(IDADR)
        ld      a,(hl)
        and     1FH             ;0Fh
        or      MULTDEF         ; tag symbol as multiple-defined
        ld      (hl),a
        pop     hl
MERROR: ld      a,'M'
        ld      (ERRFLG),a
        scf
        ret
;
PERROR: ld      a,'P'           ; phase error
        ld      (ERRFLG),a
        scf
        ret
;
;       New symbol
;
S11:    ld      hl,(SYMADR)
        inc     hl
        inc     hl
        ld      a,(hl)
        and     .not. UNDEF     ; clear UNDEF bit
        ld      (hl),a          ; define label
;
;       Now process opcode
;
S12:    call    GNC             ; fetch char after tab or space
        or      a
        ret     z               ; ignore blank line
        call    BACKUP          ; restore ptrs
        call    ID
        cp      ' '             ; test op-code separator
        jr      z,S12C
        cp      TAB
        jr      z,S12C
        or      a
        jr      z,S12C
        jr      NERROR
;
S12C:   ld      a,(IDLEN)
        or      a
        jr      z,NERROR        ; error exit

        call    FNDMAC          ; lookup macro
        jp      nc,EXPMAC       ; if found, expand it (switch context)

        call    FNDOPC          ; lookup opcode
        jr      nc,S16          ; branch if found
NERROR: ld      a,'N'
        ld      (ERRFLG),a
        ret                     ; error if undefined opcode
;
S16:    ld      a,(EVFLGS)      ; check if CPU-specific instruction
        ld      c,a
        and     Z180
        jr      z,S16A
        ld      a,(CPU)
        cp      1
        jr      nz,NERROR       ; error if wrong CPU - undefined opcode
        jr      S16B

S16A:   ld      a,c
        and     Z280
        jr      z,S16B
        ld      a,(CPU)
        cp      2
        jr      nz,NERROR       ; error if wrong CPU - undefined opcode

S16B:   ld      e,(hl)
        inc     hl
        ld      d,(hl)
        ld      (OPCODE),de     ; save opcode
        inc     hl
        ld      a,(hl)          ; get type byte
        or      a
        jp      z,NERROR        ; error if out of range
        cp      27+1
        jp      nc,NERROR
        cp      27              ; if pseudo-op
        jr      nz,S17          ; just test state if not
        ld      a,(OPCODE)      ; get opcode
        ld      c,a
        cp      8               ; is it IF?
        jr      z,S17A          ; let through always
        cp      9               ; is it ELSE?
        jr      z,S17A          ; let through always
        cp      10              ; is it ENDIF?
        jr      z,S17A          ; let through always
S17:    ld      de,(CONDSP)     ; get conditional state
        ld      a,(de)          ; get state
        rra                     ;  into carry: C=true, NC=false
        jr      c,S17A          ; if true, process the opcode
        jp      GNC             ; go past separator and finish if false
;
S17A:   call    SETLOC          ; output pending loc counter
        ld      a,(hl)          ; get type byte
        dec     a               ; -1
        ld      hl,TYPTBL
SWITCH: add     a,a             ; double for table index
        ld      e,a
        ld      d,0
        add     hl,de
        ld      a,(hl)
        inc     hl
        ld      h,(hl)
        ld      l,a
        jp      (hl)            ; dispatch to proper instruction type
;
SETLOC: ld      a,(hl)          ; get type byte
        cp      27              ; pseudo-op?
        jr      nz,STLOC        ; output pending loc counter if not
        ld      a,c
        cp      2               ; check for DB, DW, DS opcode
        ret     c               ; return if anything else
        cp      4+1
        ret     nc
STLOC:  ld      a,(LOCFLG)
        or      a               ; loc counter is pending output?
        ret     z               ; return if not
        xor     a
        ld      (LOCFLG),a      ; clear flag
        ld      a,(PASSNO)
        or      a
        ret     z               ; return if pass 1
        push    hl
        ld      a,(CURSEG)
STLC:   and     SEGMASK
        ld      a,(CURSEG)
        ld      e,a
        ld      hl,(PC)
        call    WLOC            ; else output current loc counter
        pop     hl
        ret

	psect	data

;
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

	psect	text

       SUBTTL  Instruction Class 1 - Opcode only
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
;        jr      z,S19
;        ld      c,a
;        ld      a,(CPU)
;        cp      2               ; Z280?
;        jp      nz,OERROR
;        dec     c
;        ld      b,4Eh           ; IM 3
        jp      nz,OERROR
S19:    ld      a,b
        jp      EMITB

       SUBTTL  Instruction Class 2 - Rotates
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

       SUBTTL  Instruction Class 3 - Absolute Jumps & Calls
;
;       Class 3 - Jumps - Calls
;
;	CheckRange - checks if an optimization can be made
;
;	if (VAL < PC and PC-VAL <= 126) or (VAL >= PC and VAL-PC <= 129)
;	return CARRY=1, else CARRY=0
;	B not affected
;
CheckRange:
	ld	de,(PC)		;DE=PC
	ld	hl,(VAL)	;HL=VAL
	or	a
	sbc	hl,de		;HL=VAL-PC
	jr	nc,VALGEPC
				;VAL < PC
	ex	de,hl		;HL=PC
	ld	de,(VAL)
	or	a
	sbc	hl,de		;HL=PC-VAL
	ld	c,127
	jr	compare	
VALGEPC:			;VAL >= PC, HL=VAL-PC
	ld	c,130
compare:
	ld	a,h
	or	a
	ret	nz
	ld	a,l
	cp	c
	ret

	psect	data

;
JPCOND:	defs	1
PTR:	defs	2
CODE:	defs	1
;

	psect 	text

CL3:    ld	hl,(PTR1)	;save PTR1
	ld	(PTR),hl
	call    GNC
        or      a
        jp      z,OERROR
        cp      '('
        jp      z,S53           ; branch if () form
        cp      '<'
        jp      z,S57           ; branch if <> form
        call    BACKUP
        call    EVALCND         ; try conditional
        cp      CONDD
        jp      nz,SimpleJP     ; jump if not conditional
        call    REQCHR
        defb    ','
        ld      a,l
	ld	(JPCOND),a
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
        jp      z,S56
        cp      '<'
        jp      z,S58
        ld      a,b
	ld	(CODE),a	; save opcode
        call    BACKUP
        call    EVALNEW

	ld	a,(CURSEG)	; if not in ASEG
	or	a
	ld	a,(OPMODE)
	jr	z,1f
	or	a		; do not optimize jp to abs addr
	jr	z,EMITCodeAddr
1:
	and	EXTSYM		;if jp addr is external, do not optimize
	jr	nz,EMITCodeAddr
	ld	a,(OPCODE)
	cp	0C3H
	jr	nz,EMITCodeAddr
	ld	a,(JPCOND)
	cp	4
	jr	nc,EMITCodeAddr
				; if cond = Z,NZ,C,NC
seeoptdis:
	ld	hl,JOPTDIS
	ld	a,(hl)		; 0FFH if enabled
	inc	hl		; JFLAG 0FFH if check requested
	and	(hl)		; if not both == 0FFH
	jr	z,EMITCodeAddr	; skip range checking
	inc	hl
	ld	a,(hl)		; A = JPASS
	inc	hl
	ld	b,(hl)		; B = PASSNO
	cp 	0FFH		; JPASS == 0FFH ?
	jr	nz,checkpass2	; if not, see if we are in PASS 2
	call	CheckRange	; if JPASS = 0FFH, check range
	jr	nc,EMITCodeAddr	; out range, stay with JP
	call	AddToJRtab	; in range, add PC to JR table, then...
	jr	z,EMITCodeAddr	; if table full, stay with JP
jpopt:				; else, do JP optimization
	ld	a,(PASSNO)
	or	a
	jr	z,notnow
	ld	hl,(JCOUNT)	;increment count on PASSNO=FFH
	inc	hl
	ld	(JCOUNT),hl
notnow:	
	ld	a,(JPASS)	;if in extra pass
	or	a
	call	nz,AdjustLabels	;decrement all following labels in the same seg 
	ld	hl,(PTR)	;restore source line pointer
	ld	(PTR1),hl
	ld	hl,0018H	;set opcode
	ld	(OPCODE),hl
	jp	CL4		;go to JR
;
checkpass2:
	ld	a,b
	or	a		
	jr	z,EMITCodeAddr	;if in PASS 0, continue JP handling
				;we are in PASS2, after JR optimizations were made...
	call	SearchJRtab	;is PC in the table?
	jr	nc,jpopt	;yes, apply optimization
				;else, continue JP handling
;
EMITCodeAddr:
	ld	a,(CODE)
	call	EMITB		; emit code
        jp      EMITV           ; emit address
;
SimpleJP:
	ld	a,(OPCODE)
	ld	(CODE),a    
	ld	a,(CURSEG)	; if not in ASEG
	or	a
	ld	a,(OPMODE)
	jr	z,1f
	or	a		; do not optimize jp to abs addr
	jr	z,EMITCodeAddr
1:
	and	EXTSYM		;if jp addr is external, do not optimize
	jr	nz,EMITCodeAddr
	ld	a,(OPCODE)
	cp	0C3H
	jr	nz,EMITCodeAddr
	jp	seeoptdis	;continue JP processing
;
;       JP      (HL/IX/IY)
;
S53:	;     equ     $               ; process () form
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
S56:    
;	push    bc
;       call    EVALSRG
;        pop     bc
;        cp      RSNAME
;        jr      z,S58A          ; branch if special reg
;        cp      RPNAME          ; else must be rpair
;        jp      nz,OERROR
;        ld      a,l
;        cp      4
;        jp      nz,OERROR
S54:    
	jp	OERROR
;	ld      a,(CPU)
;        cp      2
;        jp      nz,OERROR       ; Z280 only
;        ld      a,h
;        or      a
;        jp      nz,OERROR       ; IX/IY not allowed
;        ld      a,0DDh
;        call    EMITB
;        ld      a,b
S55:    call    EMITB
        call    REQCHR
        defb    ')'
        ret
;
;       CALL/JP cond,(PC+addr)  ; Z280
;
S57A:   
	jp	OERROR
;	ld      a,(OPCODE)
;        ld      b,a
;S58A:   push    bc
;        call    EVALNEW
;        pop     bc
;        ld      a,(CPU)
;        cp      2
;        jp      nz,OERROR       ; Z280 only
;        ld      a,0FDh
;        call    EMITB
;        ld      a,b
;        call    EMITB
;        call    EMITV           ; emit relative address
;        call    REQCHR
;        defb    ')'
;        ret
;
;       CALL/JP cond,<addr>     ; Z280
;
S57:    
;	ld      a,(OPCODE)
;        ld      b,a
S58:    
	jp	OERROR
;	push    bc
;        call    EVALNEW
;        pop     bc
;        ld      a,(CPU)
;        cp      2
;        jp      nz,OERROR       ; Z280 only
;        ld      a,0FDh
;        call    EMITB
;        ld      a,b
;        call    EMITB
;        ld      de,0
;        call    EMITVR          ; emit relative address
;        call    REQCHR
;        defb    '>'
;        ret

       SUBTTL  Instruction Class 4 - Relative Jumps
;
;       Class 4 - Relative jumps (JR and DJNZ, also Z280's JAF and JAR)
;
CL4:    
;	ld      a,(OPCODE+1)
;        or      a
;        jr      nz,S67          ; branch if JAF or JAR
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
;S67:    call    EMITB           ; emit prefix for JAF/JAR
;        call    EVALNEW         ; get target address
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
;        ld      a,(OPCODE+1)
;        or      a
;        jr      z,S68C
;        dec     hl              ; -3 if JAF/JAR
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

       SUBTTL  Instruction Class 5 - Restarts
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

       SUBTTL  Instruction Class 6 - Arithmetic & Logical
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

	psect	data

;
ADTBL:  defw    S92             ; (addr)
        defw    OERROR          ; (r)
        defw    S87             ; (rp)
        defw    S91             ; (rp+d8)
        defw    S87A            ; (rp+d16)  Z280
        defw    S88             ; (x+y)     Z280
        defw    S86             ; (PC+d16)  Z280
        defw    S86             ; <addr>    Z280

	psect	text

;
;       ADD     (addr)
;
S92:    
	jp	OERROR		;
;	ld      a,(CPU)
;        cp      2
;        jp      nz,OERROR       ; Z280 only
;        ld      c,87h
;        jr      S93A
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
S87A:   
	ret			;
;	ld      hl,(REGVAL)
;        ld      a,l
;        cp      6
;        jr      z,S93           ; branch if SP (Z280)
;        ld      a,h
;        or      a
;        ld      c,83h           ; HL
;        jr      z,S90
;        cp      0DDh
;        ld      c,81h           ; IX
;        jr      z,S90
;        inc     c               ; IY
;S90:    ld      a,0FDh
;        call    EMITB
;        ld      a,(OPCODE)
;        add     a,c
;        call    EMITB
;        jp      EMITV           ; emit address
;
;       ADD     (SP+d16)
;
S93:
	jp	OERROR		;
;	ld      a,(CPU)
;        cp      2               ; Z280 only
;        jp      nz,OERROR
;        ld      c,80h
;S93A:   ld      a,0DDh
;        call    EMITB
;        ld      a,(OPCODE)
;        add     a,c
;        call    EMITB
;        jp      EMITV           ; emit address
;
;       ADD     (x+y)           ; Z280
;
S88:	jp	OERROR		;
;	ld      a,0DDh
;        call    EMITB
;        ld      a,(OPCODE)
;        add     a,b
;        add     a,80h
;        jp      EMITB
;
;       ADD     <nnnn>          ; Z280
;       ADD     (PC+nnnn)       ; Z280
;
S86:	jp	OERROR		;
;	ld      a,0FDh
;        call    EMITB
;        ld      a,(OPCODE)
;        add     a,80h
;        call    EMITB
;        ld      a,c
;        cp      6
;        jp      z,EMITV         ; emit index if (PC+nnnn)
;        ld      de,0
;        jp      EMITVR          ; else emit relative address
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
S96F:   jp	OERROR		;
;	ld      a,(CPU)
;        cp      2
;        jp      nz,OERROR       ; Z280 only
;        ld      a,(VAL)
;        cp      7
;        jp      nz,OERROR       ; only reg A allowed
;        ld      a,(OPCODE+1)
;        cp      9
;        jp      nz,OERROR       ; error if not ADD
;        ld      a,0EDh
;        call    EMITB
;        ld      a,6Dh
;        jp      EMITB

       SUBTTL  Instruction Class 7 - I/O instructions
;
;       Class 7 - I/O Instructions
;
CL7:    ld      a,(OPCODE)
        ld      hl,IOTBL
        jp      SWITCH

	psect	data

;
IOTBL:  defw    ZIN             ; IN
        defw    ZOUT            ; OUT
        defw    ZIN0            ; IN0   (Z180)
        defw    ZOUT0           ; OUT0  (Z180)
        defw    ZTSTIO          ; TSTIO (Z180)
        defw    ZTSTI           ; TSTI  (Z280)
        defw    ZINW            ; INW   (Z280)
        defw    ZOUTW           ; OUTW  (Z280)

	psect 	text

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
S100:   jp	OERROR		;
;	ld      c,a
;        ld      a,(CPU)
;        cp      2
;        jp      nz,OERROR       ; Z280 only
;        ld      a,c
;        call    EVBRKT
;        push    af
;        call    CHKIOC
;        pop     af
;        ld      hl,IOTBL1
;        jp      SWITCH

;	psect	data

;
;IOTBL1: defw    S103            ; (addr)
;        defw    OERROR          ; (r)
;        defw    S100A           ; (rp)
;        defw    S100A           ; (rp+d8)
;        defw    S100A           ; (rp+d16)  Z280
;        defw    S100B           ; (x+y)     Z280
;        defw    S101            ; (PC+d16)  Z280
;        defw    S101            ; <addr>    Z280

;	psect	text

;
;S100A:  ld      a,(REGVAL)
;        cp      6
;        jr      z,S104          ; branch if SP
;        cp      4
;        jp      nz,OERROR       ; else only HL/IX/IY allowed
;        jr      S105            ; branch if no match
;
;S100B:  ld      a,0DDh
;        call    EMITB
;        ld      a,0EDh
;        call    EMITB
;        ld      a,b
;        rlca
;        rlca
;        rlca
;        ld      c,a
;        ld      a,(OPCODE+1)
;        add     a,c
;        jp      EMITB
;
;S103:   ld      a,0DDh
;        call    EMITB
;        ld      a,0EDh
;        call    EMITB
;        ld      a,(OPCODE+1)
;        add     a,38h
;        call    EMITB
;        jp      EMITV           ; emit address
;
;       IN      (SP+nnn),(C)    ; Z280
;
;S104:   ld      c,0
;        ld      a,0DDh
;        jr      S107
;
;S105:   ld      a,(REGVAL+1)
;        cp      0DDh
;        ld      c,08h
;        jr      z,S106
;        cp      0FDh
;        ld      c,10h
;        jr      z,S106
;        ld      c,18h
;S106:   ld      a,0FDh
;S107:   call    EMITB
;        ld      a,0EDh
;        call    EMITB
;        ld      a,(OPCODE+1)
;        add     a,c
;        call    EMITB
;        jp      EMITV           ; emit address
;
;       IN      <addr>,(C)      ; Z280
;       IN      (PC+addr),(C)   ; Z280
;
;S101:   ld      a,0FDh
;        call    EMITB
;        ld      a,0EDh
;        call    EMITB
;        ld      a,(OPCODE+1)
;        call    EMITB
;        ld      a,c
;        cp      6
;        jp      z,EMITV         ; emit index if (PC+addr)
;        ld      de,0
;        jp      EMITVR          ; else emit relative address
;
;       IN      HL,(C)          ; Z280
;
S102:	jp	OERROR		;
;	ld      a,(CPU)
;        cp      2
;        jp      nz,OERROR       ; Z280 only
;        ld      hl,(VAL)
;        ld      de,4
;        call    CMPHD           ; only HL allowed
;        jp      nz,OERROR
;        call    CHKIOC
;        ld      a,0EDh
;        call    EMITB
;        ld      a,0B7h
;        jp      EMITB
;
;       Test for ',(C)' operand
;
;CHKIOC: call    REQCHR
;        defb    ','
;CHKCC:  call    REQCHR
;        defb    '('
;        call    REQCHR
;        defb    'C'
;        call    REQCHR
;        defb    ')'
;        ret
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
S109A:	jp	OERROR		;
;	ld      a,(CPU)
;        cp      2
;        jp      nz,OERROR       ; Z280 only
;        ld      de,4
;        call    CMPHD
;        jp      nz,OERROR       ; only HL is valid
;        ld      a,0EDh
;        call    EMITB
;        ld      a,0BFh
;        jp      EMITB
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
S108:	jp	OERROR		;
;	ld      c,a
;        ld      a,(CPU)
;        cp      2
;        jp      nz,OERROR
;        ld      a,c
;        call    EVBRKT          ; evaluate expression in brackets
;        ld      hl,IOTBL1       ; process via common code
;        jp      SWITCH
;
;       IN0     r,(p)           ; Z180
;
ZIN0:   jp	OERROR		;
;	call    EVALREG
;        cp      RNAME           ; ensure single register
;        jp      nz,OERROR
;        ld      hl,(VAL)
;        ld      a,h
;        or      a
;        jp      nz,OERROR       ; IXH,IXL,IYH,IYL not permitted
;        ld      a,0EDh
;        call    EMITB
;        ld      a,l
;        rlca
;        rlca
;        rlca
;        call    EMITB
;        call    REQCHR
;        defb    ','
;        call    REQCHR
;        defb    '('
;        call    EVALNEW         ; get port number
;        ld      a,l
;        call    EMITB
;        call    REQCHR
;        defb    ')'
;        ret
;
;       OUT0    (p),r           ; Z180
;
ZOUT0:	jp	OERROR		;
;	call    REQCHR
;        defb    '('
;        call    EVALNEW         ; get port number
;        push    hl
;        call    REQCHR
;        defb    ')'
;        call    REQCHR
;        defb    ','
;        call    EVALREG
;        cp      RNAME           ; ensure single register
;        jp      nz,OERROR
;        ld      hl,(VAL)
;        ld      a,h
;        or      a
;        jp      nz,OERROR       ; IXH,IXL,IYH,IYL not permitted
;        ld      a,0EDh
;        call    EMITB
;        ld      a,l
;        rlca
;        rlca
;        rlca
;        or      01h
;        call    EMITB
;        pop     hl
;        ld      a,l
;        jp      EMITB
;
;       TSTIO   p               ; Z180
;
ZTSTIO: jp	OERROR		;
;	call    EVALNEW         ; get port number
;        ld      a,0EDh
;        call    EMITB
;        ld      a,74h
;        call    EMITB
;        ld      hl,(VAL)
;        ld      a,(EVMODE)
;        call    REQ8U
;        ld      a,l
;        jp      EMITB
;
;       TSTI    (C)             ; Z280
;
ZTSTI:  jp	OERROR		;
;	call    CHKCC           ; test for (C) oerand
;        ld      a,0EDh
;        call    EMITB
;        ld      a,70h
;        jp      EMITB
;
;       INW     HL,(C)          ; Z280
;
ZINW:   jp	OERROR		;
;	call    EVALREG
;        cp      RPNAME
;        jp      nz,OERROR
;        jp      S102            ; continue via common IN code
;
;       OUTW    (C),HL
;
ZOUTW:  jp	OERROR		;
;	call    CHKCC           ; syntax requires (C)
;        call    REQCHR
;        defb    ','
;        call    EVALREG
;        cp      RPNAME
;        jp      nz,OERROR
;        jp      S109A           ; continue via common OUT code
;
       SUBTTL  Instruction Class 8 - LD Instructions
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
       SUBTTL  Instruction Class 9 - PUSH & POP
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

       SUBTTL  Instruction Class 10 - Exchange Instructions
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

       SUBTTL  Instruction Class 11 - Returns
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

       SUBTTL  Instruction Class 12 - Bit Manipulation
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
       SUBTTL  Instruction Class 13 - INC & DEC
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

       SUBTTL  Instruction Class 14 - MLT, TST
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
       SUBTTL  Instruction Class 15 - ADDW, CPW, SUBW, MULTW
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
;
       SUBTTL  Instruction Class 16 - INCW, DECW
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
       SUBTTL  Instruction Class 17 - DIV, DIVU
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
       SUBTTL  Instruction Class 18 - DIVW, DIVUW
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
       SUBTTL  Instruction Class 19 - LDA (Z280)
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
       SUBTTL  Instruction Class 20 - LDCTL (Z280)
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
       SUBTTL  Instruction Class 21 - LDUD, LDUP (Z280)
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
       SUBTTL  Instruction Class 22 - MULT, MULTU (Z280)
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
       SUBTTL  Instruction Class 23 - CPL, NEG, EXTS
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

       SUBTTL  Instruction Class 24 - DI, EI
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

       SUBTTL  Instruction Class 25 - SC (Z280)
;
;       Class 25 - SC (Z280)
;
CL25:
;	call    EVALNEW
;        ld      hl,(OPCODE)
;        call    EMITW
;        jp      EMITV
;
       SUBTTL  Instruction Class 26 - EPUM, MEPU (Z280)
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
       SUBTTL  Instruction Class 27 - Pseudo-operators
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
	xor	a		;fill table with zero
	ld	b,a		;table has 256 bytes = 128 entries
setz:	ld	(hl),a
	inc	hl
	djnz	setz
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

       SUBTTL  Opcode Table
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
JRTAB:	defs	256	; space for 128 JR pointers
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



