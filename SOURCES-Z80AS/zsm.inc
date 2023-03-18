FALSE	equ	0		; fiction
TRUE	equ	1		; stranger than fiction

VER	equ	47		; version number

VER1	equ	'4'		; most significant version digit
VER2	equ	'8'		; least significant version digit

CR	equ	13
LF	equ	10
TAB	equ	9
EOF	equ	1Ah

DEFLNP	equ	60		; default lines per page, including
				;  header, title and subtitle lines

RECMAX	equ	160		; size of the input line buffer
IDMAX	equ	31		; max identifier length, must be < 32

LNGERR	equ	FALSE		; long error messages

;HDRSZ	equ	24
HDRSZ	equ	32		; min 24, must be a multiple of 8

OSTKSZ	equ	16		; size of operand and operator stacks
CSTKSZ	equ	10		; size of conditionals stack

; Symbol types (upper 3 bits of length field)

RNAME	equ	20h		; register name
RPNAME	equ	40h		; register pair name
RSNAME	equ	60h		; special register name
CONDD	equ	80h		; jump/return conditional name
EQUNAME	equ	0A0h		; EQUate label name
DFLNAME	equ	0C0h		; DEFL label name
;COMNAME equ	...		; COMMON name
MULTDEF	equ	0E0h		; multiple-defined label or equate

; Symbol modes (bit masks)

;ASGSYM = 0			; symbol in abs segment
DSGSYM	equ	080h		; symbol in data segment
CSGSYM	equ	040h		; symbol in code segment
BSGSYM	equ	0C0h		; symbol in bss segment
EXTSYM	equ	010h		; external reference (global not in local source)
GBLSYM	equ	020h		; global symbol
CUST3	equ	008h		; symbol in custom3 segment
CUST2	equ	004h		; symbol in custom2 segment
CUST1	equ	002h		; symbol in custom1 segment
UNDEF	equ	001h		; undefined symbol
;
ASG	equ	0
CSG	equ	40H
DSG	equ	80H
BSG	equ	0C0H
;
SEGMASK	equ	0CEh		; segments mask
;
; List options (bit numbers)
;
LISTFL	equ	0		; set if we want a listing
CONDFL	equ	1		; set if we want false conditionals
SYMBLS	equ	2		; set if we want a symbol table
SORTFL	equ	3		; set if we want sorted symbol table
MACRFL	equ	4		; set if we want full macro expansions
XMACFL	equ	5		; set if we want to list only macro lines
				;  that generate object code

; Opcode flags - CPU type

Z80	equ	00h
Z180	equ	10h
Z280	equ	20h

; Type of structures pushed on the dynamic storage stack

STINPF	equ	1		; include/input file state
STMDEF	equ	2		; macro definition
STMEXP	equ	3		; macro expansion/execution
STMBLD	equ	4		; macro build context (for nested macros)
STDEL	equ	0FFh		; deleted entry, awaiting garbage collection

