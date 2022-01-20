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

;	TITLE	Z80/Z180/Z280 Macro-Assembler

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
;	      without complain!!!
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

;local
        global  Z80ASM,HOFNAM,LFLAG,VAL
        global  ERRFLG,BACKUP,EVFLGS,GNC,CPU
        global  PC,PTR1,UFLAG,SYMTBL,CURSEG,CONDSP
        global  REC,LEN,IDLEN,OERROR,SYMADR,EVMODE
        global  MAXMEM,SYMPTR,SYMMOD,IDADR,GETPPC
        global  RADIX,GNR       ;,CURCMN
        global  DSPTR,MACFLG,CURLNE     ;CMNPTR
        global  DBWFLG,DSFLAG,EQUFLG,ERRCNT,HDRBUF
        global  HOFPG,IFLIST,NOLIST,LSTOPT,LENDS
        global  TITLEB,SBTTLB,RESETP
        global  LBLFLG,CNDSTK,CLEVEL,UMODE,ZERODS,DEFCPU

;external
        global  GNB,CMPHD,ID
        global  REWIND,CLSINP,CLOSE1
        global  WOBJ
        global  WDFENT,WLOC
        global  CLSOBJ,SYMPT
        global  CLSLIB
        global  INIOBJ,ADDSYM,FNDOPC

        global  LSTINI,LSTOUT,PLINE,SYMBOL,MACROS
        global  MAXLNE

        global  FNDMAC
        global  MDFLVL,MACLVL,FNDREC,MSTORE,GNRMAC
        global  EXPMAC,CVTNUM

        global  PASSNO,MODNAM,MODIDN,BSSIZE     ;CSSIZE,DSSIZE
        global  EFLG,ENDADR,ENDMOD	;,PHFLAG,UPDSIZ,SELCMN,
        global  QFLAG,COMNTC,LOCFLG,ASEGPC,CSEGPC,DSEGPC,BSEGPC
        global  LCLNUM,CMPSYM,OPCODE,TYPTBL     ;LASTCM,

        global  S552,S321,S662,SWITCH,MERROR,SETMDF,PERROR

	global	ID_OR_NUMBER
	global	TempCnt

        psect   text
;
;       Init fields
;
Z80ASM: ld      hl,(MAXMEM)
        ld      (hl),0          ; init dynamic storage
        ld      (DSPTR),hl
        ld      hl,(SYMTBL)
        ld      (SYMPT),hl
        call    RESETP          ; reset variables
        ld      a,DEFLNP
        ld      (MAXLNE),a      ; set default lines per page
        ld      (CURLNE),a      ; set up for head of form on first print
        xor     a
        ld      (PASSNO),a      ; indicate pass 1
        ld      (MDFLVL),a      ; reset MACRO definition
        ld      (MACLVL),a      ;  and expansion levels
        ld      (MODNAM),a
        ld      (MODIDN),a
        ld      hl,0
        ld      (BSSIZE),hl     ; clear BSS segment size
        call    INIOBJ
        ld      hl,HOFPG
        ld      (hl),' '
        ld      e,l
        ld      d,h
        inc     de
        ld      bc,3
        ldir                    ; clear page number
        ;continue below

;       SUBTTL  Main Assembler module
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
        ld      a,(ZERODS)      ; initialize DS to zeros?
        or      a
        jr      nz,S403         ; branch if yes
;       push    hl
;       ld      hl,(CURCMN)
;       call    SELCMN
;       pop     hl
;        ld      a,(PHFLAG)
;        or      a
;        jr      nz,S403A
        ld      a,(CURSEG)
S403A:  and     0C0h
        ld      e,a
        call    WLOC            ; else update current loc
        jr      S402
S403:   xor     a
        call    WOBJ            ; write zero bytes to initialize block
        dec     hl
        ld      a,h
        or      l
        jr      nz,S403
S402:   ld      a,(EFLG)
        or      a
        jp      z,NEXT          ; go process next record
;
;       Pass 2
;
        ld      a,(PASSNO)
        cpl
        ld      (PASSNO),a
        or      a
        jp      z,ENDIT         ; exit if finished
        call    REWIND          ; rewind input file
        xor     a
        ld      (TITLEB),a      ; clean out title
        ld      (SBTTLB),a      ;  and subtitle buffers
        ld      a,DEFLNP
        ld      (MAXLNE),a      ; force header at beginning of listing
        ld      (CURLNE),a
;       call    UPDSIZ          ; update current BSS segment size
;       ld      hl,MODNAM
;       ld      a,(hl)
;       or      a
;       jr      nz,S404
;       ld      de,HOFNAM
;       call    S552            ; use default name
;       ld      a,c
;S404:  ld      e,a
;       inc     hl
;       call    WMNAME          ; output module name

;       ld      hl,MODIDN
;       ld      a,(hl)
;       inc     hl
;       ld      e,a
;       or      a
;       call    nz,WIDENT       ; output module version identification
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
        and     0C0H            ; keep mode (text,data or bss)
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
        call    WDFENT          ;write symbol to OBJ
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
        ret                     ; exit

ERMSG:  defb    CR,LF
        defm    'Errors: '
ERMSGL  equ     $-ERMSG
;
;       RESETP - Reset variables in preparation for next pass
;
RESETP: xor     a
;        ld      (PHFLAG),a      ; reset .PHASE flag
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
;       ld      (CURCMN),hl     ; reset current
;       ld      (LASTCM),hl     ;  and selected COMMON blocks
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
        cp      ':'             ; double-colon means public label
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
;       ld      hl,(CURCMN)
;       ld      (CMNPTR),hl
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
        call    c,PERROR        ; phase error if not
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
        call    c,SETMDF        ; set multi defined flag if not
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
;       inc     hl
;       ld      de,(CURCMN)
;       ld      (hl),e          ; set pointer to current COMMON block
;       inc     hl
;       ld      (hl),d
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
;        ld      a,(PHFLAG)
;        or      a
;        jr      nz,STLC
        ld      a,(CURSEG)
STLC:   and     0C0h
        ld      a,(CURSEG)
        ld      e,a
        ld      hl,(PC)
        call    WLOC            ; else output current loc counter
        pop     hl
        ret

