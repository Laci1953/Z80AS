Tests for Z80AS

They include now the ZEXDOC & ZEXALL Z80 tests too, ready to be assembled using Z80AS.

To build them:

>Z80AS ZEXDOC
>LINK
-ptext=0 -c100h -oZEXDOC.COM zexdoc.obj
>
>Z80AS ZEXALL
>LINK
-ptext=0 -c100h -oZEXALL.COM zexall.obj
>

Each of these take 2h to complete on a 7MHz Z80
