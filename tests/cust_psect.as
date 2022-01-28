global xyz
 psect text
text1:
 ld hl,xyz
 ld hl,data1
 ld hl,bss1
 ld hl,abs1
 ld hl,aaaa1
 ld hl,bbbb1
 ld hl,xxxx1

 psect data
data1: defb 1

psect bss
bss1: defs 1

psect text,abs
org 200h
abs1: ld a,1

psect aaaa
aaaa1: ld hl,data1
 nop
 nop

psect bbb
bbbb1: ld hl,bss1
 nop

psect x
xxxx1: ld hl,text1

psect aaaa
 ld b,1

psect bbb
 ld c,1

psect x
 ld d,1

