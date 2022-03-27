psect text
nop
psect text,abs
nop
psect data,pure,global
nop
psect bss,ovrld,local
defs 1
psect aaaa,local
nop
psect bbbb,pure
nop
psect cccc
nop

; obj dump

;000A 07 0001020300015A383000
;000A 01 00000000746578740000
;0006 01 000000000000
;000A 01 00000000646174610000
;000A 01 00000000616161610000
;000A 01 00000000626262620000
;000A 01 00000000636363630000
;0003 02 D00000			ASEG 	: psect flag = D0 = abs,ovrld,global
;0005 01 0100000000
;0006 02 400062737300		BSS	: psect flag = 40 = ovrld,local
;0008 01 0100000062737300
;0007 02 30006461746100		DSEG	: psect flag = 30 = pure,global
;0009 01 010000006461746100
;0007 02 10007465787400		CSEG	: psect flag = 10 = global
;0009 01 010000007465787400
;0007 02 00006161616100		aaaa	: psect flag = 00 = local
;0009 01 010000006161616100
;0007 02 30006262626200		bbbb	: psect flag = 30 = pure,global
;0009 01 010000006262626200
;0007 02 10006363636300		cccc	: psect flag = 10 = global
;0009 01 000000006363636300
;0002 06 0000

;PSECT flag =
;(if local) 
;	(if ovrld) 40H + (if abs) 80H + (if pure) 20H
;else (global is default)
;	10H + (if ovrld) 40H + (if abs) 80H + (if pure) 20H
