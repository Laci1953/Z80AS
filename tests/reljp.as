global xxx
jp xxx

global zzz
zzz:
jp zzz

yyy equ 0
jp yyy

a1:defs 125
jp z,a1
a2:defs 126
jp z,a2
a3:defs 127
jp z,a3

JOPT OFF

a4:defs 125
jp a4
a5:defs 126
jp a5
a6:defs 127
jp a6

JOPT ON

jp a7
defs 126
a7:
jp a8
defs 127
a8:
jp a9
defs 128
a9:

jp z,a10
defs 126
a10:
jp z,a11
defs 127
a11:
jp z,a12
defs 128
a12:

jr test
defs 127
test:

test1:defs 126
jr test1

