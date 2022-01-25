; Nested MACRO definitions

        cseg
        macro test1
        db      1,2,3
        macro test2
        db      4,5,6
        endm
        db      7,8,9
        endm

        test2
        test1
        test2
        test1

        macro test3
        rept    4
        db      0
        endm
        endm

        test3

        end
