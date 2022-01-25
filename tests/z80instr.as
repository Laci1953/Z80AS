

VAL	equ	1234h

;-----------------------------------------------------------------------

	; A is optional

	ADC	A,A		; 8F
	ADC	A,B		; 88
	ADC	A,C		; 89
	ADC	A,D		; 8A
	ADC	A,E		; 8B
	ADC	A,H		; 8C
	ADC	A,L		; 8D

	ADC	A,IXH		; DD 8C
	ADC	A,IXL		; DD 8D
	ADC	A,IYH		; FD 8C
	ADC	A,IYL		; FD 8D

	ADC	A,5		; CE n

	ADC	A,(HL)		; 8E

	ADC	A,(IX)		; DD 8E 00
	ADC	A,(IX+5)	; DD 8E  d
	ADC	A,(IX-5)	; DD 8E -d
	ADC	A,(IY)		; FD 8E 00
	ADC	A,(IY+5)	; FD 8E  d
	ADC	A,(IY-5)	; FD 8E -d

;-----------------------------------------------------------------------

	ADC	HL,BC		; ED 4A
	ADC	HL,DE		; ED 5A
	ADC	HL,HL		; ED 6A
	ADC	HL,SP		; ED 7A

;-----------------------------------------------------------------------

	; A is optional

	ADD	A,A		; 87
	ADD	A,B		; 80
	ADD	A,C		; 81
	ADD	A,D		; 82
	ADD	A,E		; 83
	ADD	A,H		; 84
	ADD	A,L		; 85

	ADD	A,IXH		; DD 84
	ADD	A,IXL		; DD 85
	ADD	A,IYH		; FD 84
	ADD	A,IYL		; FD 85

	ADD	A,5		; C6 n

	ADD	A,(HL)		; 86

	ADD	A,(IX)		; DD 86 00
	ADD	A,(IX+5)	; DD 86  d
	ADD	A,(IX-5)	; DD 86 -d
	ADD	A,(IY)		; FD 86 00
	ADD	A,(IY+5)	; FD 86  d
	ADD	A,(IY-5)	; FD 86 -d

;-----------------------------------------------------------------------

	ADD	HL,BC		; 09
	ADD	HL,DE		; 19
	ADD	HL,HL		; 29
	ADD	HL,SP		; 39

	ADD	IX,BC		; DD 09
	ADD	IX,DE		; DD 19
	ADD	IX,IX		; DD 29
	ADD	IX,SP		; DD 39

	ADD	IY,BC		; FD 09
	ADD	IY,DE		; FD 19
	ADD	IY,IY		; FD 29
	ADD	IY,SP		; FD 39

;-----------------------------------------------------------------------

	; A is optional

	AND	A,A		; A7
	AND	A,B		; A0
	AND	A,C		; A1
	AND	A,D		; A2
	AND	A,E		; A3
	AND	A,H		; A4
	AND	A,L		; A5

	AND	A,IXH		; DD A4
	AND	A,IXL		; DD A5
	AND	A,IYH		; FD A4
	AND	A,IYL		; FD A5

	AND	A,5		; E6 n

	AND	A,(HL)		; A6

	AND	A,(IX)		; DD A6 00
	AND	A,(IX+5)	; DD A6  d
	AND	A,(IX-5)	; DD A6 -d
	AND	A,(IY)		; FD A6 00
	AND	A,(IY+5)	; FD A6  d
	AND	A,(IY-5)	; FD A6 -d

;-----------------------------------------------------------------------

	BIT	0,A		; CB 47
	BIT	0,B		; CB 40
	BIT	0,C		; CB 41
	BIT	0,D		; CB 42
	BIT	0,E		; CB 43
	BIT	0,H		; CB 44
	BIT	0,L		; CB 45
	BIT	0,(HL)		; CB 46

	BIT	1,A		; CB 4F
	BIT	1,B		; CB 48
	BIT	1,C		; CB 49
	BIT	1,D		; CB 4A
	BIT	1,E		; CB 4B
	BIT	1,H		; CB 4C
	BIT	1,L		; CB 4D
	BIT	1,(HL)		; CB 4E

	BIT	2,A		; CB 57
	BIT	2,B		; CB 50
	BIT	2,C		; CB 51
	BIT	2,D		; CB 52
	BIT	2,E		; CB 53
	BIT	2,H		; CB 54
	BIT	2,L		; CB 55
	BIT	2,(HL)		; CB 56

	BIT	3,A		; CB 5F
	BIT	3,B		; CB 58
	BIT	3,C		; CB 59
	BIT	3,D		; CB 5A
	BIT	3,E		; CB 5B
	BIT	3,H		; CB 5C
	BIT	3,L		; CB 5D
	BIT	3,(HL)		; CB 5E

	BIT	4,A		; CB 67
	BIT	4,B		; CB 60
	BIT	4,C		; CB 61
	BIT	4,D		; CB 62
	BIT	4,E		; CB 63
	BIT	4,H		; CB 64
	BIT	4,L		; CB 65
	BIT	4,(HL)		; CB 66

	BIT	5,A		; CB 6F
	BIT	5,B		; CB 68
	BIT	5,C		; CB 69
	BIT	5,D		; CB 6A
	BIT	5,E		; CB 6B
	BIT	5,H		; CB 6C
	BIT	5,L		; CB 6D
	BIT	5,(HL)		; CB 6E

	BIT	6,A		; CB 77
	BIT	6,B		; CB 70
	BIT	6,C		; CB 71
	BIT	6,D		; CB 72
	BIT	6,E		; CB 73
	BIT	6,H		; CB 74
	BIT	6,L		; CB 75
	BIT	6,(HL)		; CB 76

	BIT	7,A		; CB 7F
	BIT	7,B		; CB 78
	BIT	7,C		; CB 79
	BIT	7,D		; CB 7A
	BIT	7,E		; CB 7B
	BIT	7,H		; CB 7C
	BIT	7,L		; CB 7D
	BIT	7,(HL)		; CB 7E

	BIT	0,(IX)		; DD CB 00 46
	BIT	0,(IX+5)	; DD CB  d 46
	BIT	0,(IX-5)	; DD CB -d 46
	BIT	0,(IY)		; FD CB 00 46
	BIT	0,(IY+5)	; FD CB  d 46
	BIT	0,(IY-5)	; FD CB -d 46

	BIT	1,(IX)		; DD CB 00 4E
	BIT	1,(IX+5)	; DD CB  d 4E
	BIT	1,(IX-5)	; DD CB -d 4E
	BIT	1,(IY)		; FD CB 00 4E
	BIT	1,(IY+5)	; FD CB  d 4E
	BIT	1,(IY-5)	; FD CB -d 4E

	BIT	2,(IX)		; DD CB 00 56
	BIT	2,(IX+5)	; DD CB  d 56
	BIT	2,(IX-5)	; DD CB -d 56
	BIT	2,(IY)		; FD CB 00 56
	BIT	2,(IY+5)	; FD CB  d 56
	BIT	2,(IY-5)	; FD CB -d 56

	BIT	3,(IX)		; DD CB 00 5E
	BIT	3,(IX+5)	; DD CB  d 5E
	BIT	3,(IX-5)	; DD CB -d 5E
	BIT	3,(IY)		; FD CB 00 5E
	BIT	3,(IY+5)	; FD CB  d 5E
	BIT	3,(IY-5)	; FD CB -d 5E

	BIT	4,(IX)		; DD CB 00 66
	BIT	4,(IX+5)	; DD CB  d 66
	BIT	4,(IX-5)	; DD CB -d 66
	BIT	4,(IY)		; FD CB 00 66
	BIT	4,(IY+5)	; FD CB  d 66
	BIT	4,(IY-5)	; FD CB -d 66

	BIT	5,(IX)		; DD CB 00 6E
	BIT	5,(IX+5)	; DD CB  d 6E
	BIT	5,(IX-5)	; DD CB -d 6E
	BIT	5,(IY)		; FD CB 00 6E
	BIT	5,(IY+5)	; FD CB  d 6E
	BIT	5,(IY-5)	; FD CB -d 6E

	BIT	6,(IX)		; DD CB 00 76
	BIT	6,(IX+5)	; DD CB  d 76
	BIT	6,(IX-5)	; DD CB -d 76
	BIT	6,(IY)		; FD CB 00 76
	BIT	6,(IY+5)	; FD CB  d 76
	BIT	6,(IY-5)	; FD CB -d 76

	BIT	7,(IX)		; DD CB 00 7E
	BIT	7,(IX+5)	; DD CB  d 7E
	BIT	7,(IX-5)	; DD CB -d 7E
	BIT	7,(IY)		; FD CB 00 7E
	BIT	7,(IY+5)	; FD CB  d 7E
	BIT	7,(IY-5)	; FD CB -d 7E

;-----------------------------------------------------------------------

	CALL	NZ,ADDR		; C4 addr
	CALL	Z,ADDR		; CC addr
	CALL	NC,ADDR		; D4 addr
	CALL	C,ADDR		; DC addr
	CALL	PO,ADDR		; E4 addr
	CALL	PE,ADDR		; EC addr
	CALL	NV,ADDR		; E4 addr
	CALL	V,ADDR		; EC addr
	CALL	P,ADDR		; F4 addr
	CALL	M,ADDR		; FC addr
	CALL	NS,ADDR		; F4 addr
	CALL	S,ADDR		; FC addr

	CALL	ADDR		; CD addr

;-----------------------------------------------------------------------

	CCF			; 3F

;-----------------------------------------------------------------------

	; A is optional

	CP	A,A		; BF
	CP	A,B		; B8
	CP	A,C		; B9
	CP	A,D		; BA
	CP	A,E		; BB
	CP	A,H		; BC
	CP	A,L		; BD

	CP	A,IXH		; DD BC
	CP	A,IXL		; DD BD
	CP	A,IYH		; FD BC
	CP	A,IYL		; FD BD

	CP	A,5		; FE n

	CP	A,(HL)		; BE

	CP	A,(IX)		; DD BE 00
	CP	A,(IX+5)	; DD BE  d
	CP	A,(IX-5)	; DD BE -d
	CP	A,(IY)		; FD BE 00
	CP	A,(IY+5)	; FD BE  d
	CP	A,(IY-5)	; FD BE -d

;-----------------------------------------------------------------------

	CPD			; ED A9
	CPDR			; ED B9

	CPI			; ED A1
	CPIR			; ED B1

;-----------------------------------------------------------------------

	; A is optional

	CPL	A		; 2F

;-----------------------------------------------------------------------

	DAA			; 27

;-----------------------------------------------------------------------

	DEC	A		; 3D
	DEC	B		; 05
	DEC	C		; 0D
	DEC	D		; 15
	DEC	E		; 1D
	DEC	H		; 25
	DEC	L		; 2D

	DEC	IXH		; DD 25
	DEC	IXL		; DD 2D
	DEC	IYH		; FD 25
	DEC	IYL		; FD 2D

	DEC	(HL)		; 35

	DEC	(IX)		; DD 35 00
	DEC	(IX+5)		; DD 35  d
	DEC	(IX-5)		; DD 35 -d
	DEC	(IY)		; FD 35 00
	DEC	(IY+5)		; FD 35  d
	DEC	(IY-5)		; FD 35 -d

	DEC	BC		; 0B
	DEC	DE		; 1B
	DEC	HL		; 2B
	DEC	SP		; 3B

	DEC	IX		; DD 2B
	DEC	IY		; FD 2B

;-----------------------------------------------------------------------

	DI			; F3

;-----------------------------------------------------------------------

	DJNZ	$+5		; 10 d

;-----------------------------------------------------------------------

	EI			; FB

;-----------------------------------------------------------------------

	EX	AF,AF'		; 08

	EX	(SP),HL		; E3
	EX	(SP),IX		; DD E3
	EX	(SP),IY		; FD E3

	EX	DE,HL		; EB

;-----------------------------------------------------------------------

	EXX			; D9

;-----------------------------------------------------------------------

	HALT			; 76

;-----------------------------------------------------------------------

	IM	0		; ED 46
	IM	1		; ED 56
	IM	2		; ED 5E

;-----------------------------------------------------------------------

	IN	A,(C)		; ED 78
	IN	B,(C)		; ED 40
	IN	C,(C)		; ED 48
	IN	D,(C)		; ED 50
	IN	E,(C)		; ED 58
	IN	H,(C)		; ED 60
	IN	L,(C)		; ED 68

	IN	IXH,(C)		; DD ED 60
	IN	IXL,(C)		; DD ED 68
	IN	IYH,(C)		; FD ED 60
	IN	IYL,(C)		; FD ED 68

	IN	A,(5)		; DB n

;-----------------------------------------------------------------------

	INC	A		; 3C
	INC	B		; 04
	INC	C		; 0C
	INC	D		; 14
	INC	E		; 1C
	INC	H		; 24
	INC	L		; 2C

	INC	IXH		; DD 24
	INC	IXL		; DD 2C
	INC	IYH		; FD 24
	INC	IYL		; FD 2C

	INC	(HL)		; 34

	INC	(IX)		; DD 34 00
	INC	(IX+5)		; DD 34  d
	INC	(IX-5)		; DD 34 -d
	INC	(IY)		; FD 34 00
	INC	(IY+5)		; FD 34  d
	INC	(IY-5)		; FD 34 -d

	INC	BC		; 03
	INC	DE		; 13
	INC	HL		; 23
	INC	SP		; 33

	INC	IX		; DD 23
	INC	IY		; FD 23

;-----------------------------------------------------------------------

	IND			; ED AA 
	INDR			; ED BA

	INI			; ED A2
	INIR			; ED B2

;-----------------------------------------------------------------------

	JP	(HL)		; E9

	JP	(IX)		; DD E9
	JP	(IY)		; FD E9

	JP	NZ,ADDR		; C2 addr
	JP	Z,ADDR		; CA addr
	JP	NC,ADDR		; D2 addr
	JP	C,ADDR		; DA addr
	JP	PO,ADDR		; E2 addr
	JP	PE,ADDR		; EA addr
	JP	NV,ADDR		; E2 addr
	JP	V,ADDR		; EA addr
	JP	P,ADDR		; F2 addr
	JP	M,ADDR		; FA addr
	JP	NS,ADDR		; F2 addr
	JP	S,ADDR		; FA addr

	JP	ADDR		; C3 addr

;-----------------------------------------------------------------------

	JR	NZ,$+5		; 20 d
	JR	Z,$+5		; 28 d
	JR	NC,$+5		; 30 d
	JR	C,$+5		; 38 d

;-----------------------------------------------------------------------

	LD	A,A		; 7F
	LD	A,B		; 78
	LD	A,C		; 79
	LD	A,D		; 7A
	LD	A,E		; 7B
	LD	A,H		; 7C
	LD	A,L		; 7D

	LD	A,IXH		; DD 7C
	LD	A,IXL		; DD 7D
	LD	A,IYH		; FD 7C
	LD	A,IYL		; FD 7D

	LD	A,5		; 3E n

	LD	A,(BC)		; 0A
	LD	A,(DE)		; 1A

	LD	A,(HL)		; 7E

	LD	A,(ADDR)	; 3A addr

	LD	A,(IX)		; DD 7E 00
	LD	A,(IX+5)	; DD 7E  d
	LD	A,(IX-5)	; DD 7E -d
	LD	A,(IY)		; FD 7E 00
	LD	A,(IY+5)	; FD 7E  d
	LD	A,(IY-5)	; FD 7E -d

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

	LD	A,A		; 7F
	LD	B,A		; 47
	LD	C,A		; 4F
	LD	D,A		; 57
	LD	E,A		; 5F
	LD	H,A		; 67
	LD	L,A		; 6F

	LD	IXH,A		; DD 67
	LD	IXL,A		; DD 6F
	LD	IYH,A		; FD 67
	LD	IYL,A		; FD 6F

	LD	(BC),A		; 02
	LD	(DE),A		; 12

	LD	(HL),A		; 77

	LD	(ADDR),A	; 32 addr

	LD	(IX),A		; DD 77 00
	LD	(IX+5),A	; DD 77  d
	LD	(IX-5),A	; DD 77 -d
	LD	(IY),A		; FD 77 00
	LD	(IY+5),A	; FD 77  d
	LD	(IY-5),A	; FD 77 -d

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

	LD	A,I		; ED 57
	LD	A,R		; ED 5F

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

	LD	A,5		; 3E n
	LD	B,5		; 06 n
	LD	C,5		; 0E n
	LD	D,5		; 16 n
	LD	E,5		; 1E n
	LD	H,5		; 26 n
	LD	L,5		; 2E n

	LD	IXH,5		; DD 26 n
	LD	IXL,5		; DD 2E n
	LD	IYH,5		; FD 26 n
	LD	IYL,5		; FD 2E n

	LD	(HL),5		; 36 n

	LD	(IX),5		; DD 36 00 n
	LD	(IX+5),5	; DD 36  d n
	LD	(IX-5),5	; DD 36 -d n
	LD	(IY),5		; FD 36 00 n
	LD	(IY+5),5	; FD 36  d n
	LD	(IY-5),5	; FD 36 -d n

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

	LD	A,A		; 7F
	LD	A,B		; 78
	LD	A,C		; 79
	LD	A,D		; 7A
	LD	A,E		; 7B
	LD	A,H		; 7C
	LD	A,L		; 7D

	LD	B,A		; 47
	LD	B,B		; 40
	LD	B,C		; 41
	LD	B,D		; 42
	LD	B,E		; 43
	LD	B,H		; 44
	LD	B,L		; 45

	LD	C,A		; 4F
	LD	C,B		; 48
	LD	C,C		; 49
	LD	C,D		; 4A
	LD	C,E		; 4B
	LD	C,H		; 4C
	LD	C,L		; 4D

	LD	D,A		; 57
	LD	D,B		; 50
	LD	D,C		; 51
	LD	D,D		; 52
	LD	D,E		; 53
	LD	D,H		; 54
	LD	D,L		; 55

	LD	E,A		; 5F
	LD	E,B		; 58
	LD	E,C		; 59
	LD	E,D		; 5A
	LD	E,E		; 5B
	LD	E,H		; 5C
	LD	E,L		; 5D

	LD	H,A		; 67
	LD	H,B		; 60
	LD	H,C		; 61
	LD	H,D		; 62
	LD	H,E		; 63
	LD	H,H		; 64
	LD	H,L		; 65

	LD	L,A		; 6F
	LD	L,B		; 68
	LD	L,C		; 69
	LD	L,D		; 6A
	LD	L,E		; 6B
	LD	L,H		; 6C
	LD	L,L		; 6D

	LD	A,IXH		; DD 7C
	LD	A,IXL		; DD 7D
	LD	A,IYH		; FD 7C
	LD	A,IYL		; FD 7D

	LD	B,IXH		; DD 44
	LD	B,IXL		; DD 45
	LD	B,IYH		; FD 44
	LD	B,IYL		; FD 45

	LD	C,IXH		; DD 4C
	LD	C,IXL		; DD 4D
	LD	C,IYH		; FD 4C
	LD	C,IYL		; FD 4D

	LD	D,IXH		; DD 54
	LD	D,IXL		; DD 55
	LD	D,IYH		; FD 54
	LD	D,IYL		; FD 55

	LD	E,IXH		; DD 5C
	LD	E,IXL		; DD 5D
	LD	E,IYH		; FD 5C
	LD	E,IYL		; FD 5D

	LD	IXH,A		; DD 67
	LD	IXL,A		; DD 6F
	LD	IYH,A		; FD 67
	LD	IYL,A		; FD 6F

	LD	IXH,B		; DD 60
	LD	IXL,B		; DD 68
	LD	IYH,B		; FD 60
	LD	IYL,B		; FD 68

	LD	IXH,C		; DD 61
	LD	IXL,C		; DD 69
	LD	IYH,C		; FD 61
	LD	IYL,C		; FD 69

	LD	IXH,D		; DD 62
	LD	IXL,D		; DD 6A
	LD	IYH,D		; FD 62
	LD	IYL,D		; FD 6A

	LD	IXH,E		; DD 63
	LD	IXL,E		; DD 6B
	LD	IYH,E		; FD 63
	LD	IYL,E		; FD 6B

	LD	IXH,IXH		; DD 64
	LD	IXH,IXL		; DD 65
	LD	IXL,IXH		; DD 6C
	LD	IXL,IXL		; DD 6D

	LD	IYH,IYH		; FD 64
	LD	IYH,IYL		; FD 65
	LD	IYL,IYH		; FD 6C
	LD	IYL,IYL		; FD 6D

	LD	A,(HL)		; 7E
	LD	B,(HL)		; 46
	LD	C,(HL)		; 4E
	LD	D,(HL)		; 56
	LD	E,(HL)		; 5E
	LD	H,(HL)		; 66
	LD	L,(HL)		; 6E

	LD	(HL),A		; 77
	LD	(HL),B		; 70
	LD	(HL),C		; 71
	LD	(HL),D		; 72
	LD	(HL),E		; 73
	LD	(HL),H		; 74
	LD	(HL),L		; 75

	LD	A,(IX)		; DD 7E 00
	LD	B,(IX)		; DD 46 00
	LD	C,(IX)		; DD 4E 00
	LD	D,(IX)		; DD 56 00
	LD	E,(IX)		; DD 5E 00
	LD	H,(IX)		; DD 66 00
	LD	L,(IX)		; DD 6E 00

	LD	A,(IX+5)	; DD 7E  d
	LD	B,(IX+5)	; DD 46  d
	LD	C,(IX+5)	; DD 4E  d
	LD	D,(IX+5)	; DD 56  d
	LD	E,(IX+5)	; DD 5E  d
	LD	H,(IX+5)	; DD 66  d
	LD	L,(IX+5)	; DD 6E  d

	LD	A,(IX-5)	; DD 7E -d
	LD	B,(IX-5)	; DD 46 -d
	LD	C,(IX-5)	; DD 4E -d
	LD	D,(IX-5)	; DD 56 -d
	LD	E,(IX-5)	; DD 5E -d
	LD	H,(IX-5)	; DD 66 -d
	LD	L,(IX-5)	; DD 6E -d

	LD	A,(IY)		; FD 7E 00
	LD	B,(IY)		; FD 46 00
	LD	C,(IY)		; FD 4E 00
	LD	D,(IY)		; FD 56 00
	LD	E,(IY)		; FD 5E 00
	LD	H,(IY)		; FD 66 00
	LD	L,(IY)		; FD 6E 00

	LD	A,(IY+5)	; FD 7E  d
	LD	B,(IY+5)	; FD 46  d
	LD	C,(IY+5)	; FD 4E  d
	LD	D,(IY+5)	; FD 56  d
	LD	E,(IY+5)	; FD 5E  d
	LD	H,(IY+5)	; FD 66  d
	LD	L,(IY+5)	; FD 6E  d

	LD	A,(IY-5)	; FD 7E -d
	LD	B,(IY-5)	; FD 46 -d
	LD	C,(IY-5)	; FD 4E -d
	LD	D,(IY-5)	; FD 56 -d
	LD	E,(IY-5)	; FD 5E -d
	LD	H,(IY-5)	; FD 66 -d
	LD	L,(IY-5)	; FD 6E -d

	LD	(IX),A		; DD 77 00
	LD	(IX),B		; DD 70 00
	LD	(IX),C		; DD 71 00
	LD	(IX),D		; DD 72 00
	LD	(IX),E		; DD 73 00
	LD	(IX),H		; DD 74 00
	LD	(IX),L		; DD 75 00

	LD	(IX+5),A	; DD 77  d
	LD	(IX+5),B	; DD 70  d
	LD	(IX+5),C	; DD 71  d
	LD	(IX+5),D	; DD 72  d
	LD	(IX+5),E	; DD 73  d
	LD	(IX+5),H	; DD 74  d
	LD	(IX+5),L	; DD 75  d

	LD	(IX-5),A	; DD 77 -d
	LD	(IX-5),B	; DD 70 -d
	LD	(IX-5),C	; DD 71 -d
	LD	(IX-5),D	; DD 72 -d
	LD	(IX-5),E	; DD 73 -d
	LD	(IX-5),H	; DD 74 -d
	LD	(IX-5),L	; DD 75 -d

	LD	(IY),A		; FD 77 00
	LD	(IY),B		; FD 70 00
	LD	(IY),C		; FD 71 00
	LD	(IY),D		; FD 72 00
	LD	(IY),E		; FD 73 00
	LD	(IY),H		; FD 74 00
	LD	(IY),L		; FD 75 00

	LD	(IY+5),A	; FD 77  d
	LD	(IY+5),B	; FD 70  d
	LD	(IY+5),C	; FD 71  d
	LD	(IY+5),D	; FD 72  d
	LD	(IY+5),E	; FD 73  d
	LD	(IY+5),H	; FD 74  d
	LD	(IY+5),L	; FD 75  d

	LD	(IY-5),A	; FD 77 -d
	LD	(IY-5),B	; FD 70 -d
	LD	(IY-5),C	; FD 71 -d
	LD	(IY-5),D	; FD 72 -d
	LD	(IY-5),E	; FD 73 -d
	LD	(IY-5),H	; FD 74 -d
	LD	(IY-5),L	; FD 75 -d

;- - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - - 

	LD	I,A		; ED 47
	LD	R,A		; ED 4F

;-----------------------------------------------------------------------

	LDD			; ED A8
	LDDR			; ED B8

	LDI			; ED A0
	LDIR			; ED B0

;-----------------------------------------------------------------------

	LD	BC,VAL		; 01 val
	LD	DE,VAL		; 11 val
	LD	HL,VAL		; 21 val
	LD	SP,VAL		; 31 val

	LD	IX,VAL		; DD 21 val
	LD	IY,VAL		; FD 21 val

	LD	BC,(ADDR)	; ED 4B addr
	LD	DE,(ADDR)	; ED 5B addr
	LD	HL,(ADDR)	; 2A addr
	LD	SP,(ADDR)	; ED 7B addr

	LD	IX,(ADDR)	; DD 2A addr
	LD	IY,(ADDR)	; FD 2A addr

	LD	(ADDR),BC	; ED 43 addr
	LD	(ADDR),DE	; ED 53 addr
	LD	(ADDR),HL	; 22 addr
	LD	(ADDR),SP	; ED 73 addr

	LD	(ADDR),IX	; DD 22 addr
	LD	(ADDR),IY	; FD 22 addr

	LD	SP,HL		; F9
	LD	SP,IX		; DD F9
	LD	SP,IY		; FD F9

;-----------------------------------------------------------------------

	; A is optional

	NEG	A		; ED 44

;-----------------------------------------------------------------------

	NOP			; 00

;-----------------------------------------------------------------------

	; A is optional

	OR	A,A		; B7
	OR	A,B		; B0
	OR	A,C		; B1
	OR	A,D		; B2
	OR	A,E		; B3
	OR	A,H		; B4
	OR	A,L		; B5

	OR	A,IXH		; DD B4
	OR	A,IXL		; DD B5
	OR	A,IYH		; FD B4
	OR	A,IYL		; FD B5

	OR	A,5		; F6 n

	OR	A,(HL)		; B6

	OR	A,(IX)		; DD B6 00
	OR	A,(IX+5)	; DD B6  d
	OR	A,(IX-5)	; DD B6 -d
	OR	A,(IY)		; FD B6 00
	OR	A,(IY+5)	; FD B6  d
	OR	A,(IY-5)	; FD B6 -d

;-----------------------------------------------------------------------

	OUTD			; ED AB
	OTDR			; ED BB

	OUTI			; ED A3
	OTIR			; ED B3

;-----------------------------------------------------------------------

	OUT	(C),A		; ED 79
	OUT	(C),B		; ED 41
	OUT	(C),C		; ED 49
	OUT	(C),D		; ED 51
	OUT	(C),E		; ED 59
	OUT	(C),H		; ED 61
	OUT	(C),L		; ED 69

	OUT	(C),IXH		; DD ED 61
	OUT	(C),IXL		; DD ED 69
	OUT	(C),IYH		; FD ED 61
	OUT	(C),IYL		; FD ED 69

	OUT	(5),A		; D3 n

;-----------------------------------------------------------------------

	POP	AF		; F1
	POP	BC		; C1
	POP	DE		; D1
	POP	HL		; E1

	POP	IX		; DD E1
	POP	IY		; FD E1

;-----------------------------------------------------------------------

	PUSH	AF		; F5
	PUSH	BC		; C5
	PUSH	DE		; D5
	PUSH	HL		; E5

	PUSH	IX		; DD E5
	PUSH	IY		; FD E5

;-----------------------------------------------------------------------

	RES	0,A		; CB 87
	RES	0,B		; CB 80
	RES	0,C		; CB 81
	RES	0,D		; CB 82
	RES	0,E		; CB 83
	RES	0,H		; CB 84
	RES	0,L		; CB 85
	RES	0,(HL)		; CB 86

	RES	1,A		; CB 8F
	RES	1,B		; CB 88
	RES	1,C		; CB 89
	RES	1,D		; CB 8A
	RES	1,E		; CB 8B
	RES	1,H		; CB 8C
	RES	1,L		; CB 8D
	RES	1,(HL)		; CB 8E

	RES	2,A		; CB 97
	RES	2,B		; CB 90
	RES	2,C		; CB 91
	RES	2,D		; CB 92
	RES	2,E		; CB 93
	RES	2,H		; CB 94
	RES	2,L		; CB 95
	RES	2,(HL)		; CB 96

	RES	3,A		; CB 9F
	RES	3,B		; CB 98
	RES	3,C		; CB 99
	RES	3,D		; CB 9A
	RES	3,E		; CB 9B
	RES	3,H		; CB 9C
	RES	3,L		; CB 9D
	RES	3,(HL)		; CB 9E

	RES	4,A		; CB A7
	RES	4,B		; CB A0
	RES	4,C		; CB A1
	RES	4,D		; CB A2
	RES	4,E		; CB A3
	RES	4,H		; CB A4
	RES	4,L		; CB A5
	RES	4,(HL)		; CB A6

	RES	5,A		; CB AF
	RES	5,B		; CB A8
	RES	5,C		; CB A9
	RES	5,D		; CB AA
	RES	5,E		; CB AB
	RES	5,H		; CB AC
	RES	5,L		; CB AD
	RES	5,(HL)		; CB AE

	RES	6,A		; CB B7
	RES	6,B		; CB B0
	RES	6,C		; CB B1
	RES	6,D		; CB B2
	RES	6,E		; CB B3
	RES	6,H		; CB B4
	RES	6,L		; CB B5
	RES	6,(HL)		; CB B6

	RES	7,A		; CB BF
	RES	7,B		; CB B8
	RES	7,C		; CB B9
	RES	7,D		; CB BA
	RES	7,E		; CB BB
	RES	7,H		; CB BC
	RES	7,L		; CB BD
	RES	7,(HL)		; CB BE

	RES	0,(IX)		; DD CB 00 86
	RES	0,(IX+5)	; DD CB  d 86
	RES	0,(IX-5)	; DD CB -d 86
	RES	0,(IY)		; FD CB 00 86
	RES	0,(IY+5)	; FD CB  d 86
	RES	0,(IY-5)	; FD CB -d 86

	RES	1,(IX)		; DD CB 00 8E
	RES	1,(IX+5)	; DD CB  d 8E
	RES	1,(IX-5)	; DD CB -d 8E
	RES	1,(IY)		; FD CB 00 8E
	RES	1,(IY+5)	; FD CB  d 8E
	RES	1,(IY-5)	; FD CB -d 8E

	RES	2,(IX)		; DD CB 00 96
	RES	2,(IX+5)	; DD CB  d 96
	RES	2,(IX-5)	; DD CB -d 96
	RES	2,(IY)		; FD CB 00 96
	RES	2,(IY+5)	; FD CB  d 96
	RES	2,(IY-5)	; FD CB -d 96

	RES	3,(IX)		; DD CB 00 9E
	RES	3,(IX+5)	; DD CB  d 9E
	RES	3,(IX-5)	; DD CB -d 9E
	RES	3,(IY)		; FD CB 00 9E
	RES	3,(IY+5)	; FD CB  d 9E
	RES	3,(IY-5)	; FD CB -d 9E

	RES	4,(IX)		; DD CB 00 A6
	RES	4,(IX+5)	; DD CB  d A6
	RES	4,(IX-5)	; DD CB -d A6
	RES	4,(IY)		; FD CB 00 A6
	RES	4,(IY+5)	; FD CB  d A6
	RES	4,(IY-5)	; FD CB -d A6

	RES	5,(IX)		; DD CB 00 AE
	RES	5,(IX+5)	; DD CB  d AE
	RES	5,(IX-5)	; DD CB -d AE
	RES	5,(IY)		; FD CB 00 AE
	RES	5,(IY+5)	; FD CB  d AE
	RES	5,(IY-5)	; FD CB -d AE

	RES	6,(IX)		; DD CB 00 B6
	RES	6,(IX+5)	; DD CB  d B6
	RES	6,(IX-5)	; DD CB -d B6
	RES	6,(IY)		; FD CB 00 B6
	RES	6,(IY+5)	; FD CB  d B6
	RES	6,(IY-5)	; FD CB -d B6

	RES	7,(IX)		; DD CB 00 BE
	RES	7,(IX+5)	; DD CB  d BE
	RES	7,(IX-5)	; DD CB -d BE
	RES	7,(IY)		; FD CB 00 BE
	RES	7,(IY+5)	; FD CB  d BE
	RES	7,(IY-5)	; FD CB -d BE

;-----------------------------------------------------------------------

	RET	NZ		; C0
	RET	Z		; C8
	RET	NC		; D0
	RET	C		; D8
	RET	PO		; E0
	RET	PE		; E8
	RET	NV		; E0
	RET	V		; E8
	RET	P		; F0
	RET	M		; F8
	RET	NS		; F0
	RET	S		; F8

	RET			; C9

;-----------------------------------------------------------------------

	RETI			; ED 4D

;-----------------------------------------------------------------------

	RETN			; ED 45

;-----------------------------------------------------------------------

	RL	A		; CB 17
	RL	B		; CB 10
	RL	C		; CB 11
	RL	D		; CB 12
	RL	E		; CB 13
	RL	H		; CB 14
	RL	L		; CB 15

	RL	(HL)		; CB 16

	RL	(IX)		; DD CB 00 16
	RL	(IX+5)		; DD CB  d 16
	RL	(IX-5)		; DD CB -d 16
	RL	(IY)		; FD CB 00 16
	RL	(IY+5)		; FD CB  d 16
	RL	(IY-5)		; FD CB -d 16

;-----------------------------------------------------------------------

	RLA			; 17

;-----------------------------------------------------------------------

	RLC	A		; CB 07
	RLC	B		; CB 00
	RLC	C		; CB 01
	RLC	D		; CB 02
	RLC	E		; CB 03
	RLC	H		; CB 04
	RLC	L		; CB 05

	RLC	(HL)		; CB 06

	RLC	(IX)		; DD CB 00 06
	RLC	(IX+5)		; DD CB  d 06
	RLC	(IX-5)		; DD CB -d 06
	RLC	(IY)		; FD CB 00 06
	RLC	(IY+5)		; FD CB  d 06
	RLC	(IY-5)		; FD CB -d 06

;-----------------------------------------------------------------------

	RLCA			; 07

;-----------------------------------------------------------------------

	RLD			; ED 6F

;-----------------------------------------------------------------------

	RR	A		; CB 1F
	RR	B		; CB 18
	RR	C		; CB 19
	RR	D		; CB 1A
	RR	E		; CB 1B
	RR	H		; CB 1C
	RR	L		; CB 1D

	RR	(HL)		; CB 1E

	RR	(IX)		; DD CB 00 1E
	RR	(IX+5)		; DD CB  d 1E
	RR	(IX-5)		; DD CB -d 1E
	RR	(IY)		; FD CB 00 1E
	RR	(IY+5)		; FD CB  d 1E
	RR	(IY-5)		; FD CB -d 1E

;-----------------------------------------------------------------------

	RRA			; 1F

;-----------------------------------------------------------------------

	RRC	A		; CB 0F
	RRC	B		; CB 08
	RRC	C		; CB 09
	RRC	D		; CB 0A
	RRC	E		; CB 0B
	RRC	H		; CB 0C
	RRC	L		; CB 0D

	RRC	(HL)		; CB 0E

	RRC	(IX)		; DD CB 00 0E
	RRC	(IX+5)		; DD CB  d 0E
	RRC	(IX-5)		; DD CB -d 0E
	RRC	(IY)		; FD CB 00 0E
	RRC	(IY+5)		; FD CB  d 0E
	RRC	(IY-5)		; FD CB -d 0E

;-----------------------------------------------------------------------

	RRCA			; 0F

;-----------------------------------------------------------------------

	RRD			; ED 67

;-----------------------------------------------------------------------

	RST	00h		; C7
	RST	08h		; CF
	RST	10h		; D7
	RST	18h		; DF
	RST	20h		; E7
	RST	28h		; EF
	RST	30h		; F7
	RST	38h		; FF

;-----------------------------------------------------------------------

	; A is optional

	SBC	A,A		; 9F
	SBC	A,B		; 98
	SBC	A,C		; 99
	SBC	A,D		; 9A
	SBC	A,E		; 9B
	SBC	A,H		; 9C
	SBC	A,L		; 9D

	SBC	A,IXH		; DD 9C
	SBC	A,IXL		; DD 9D
	SBC	A,IYH		; FD 9C
	SBC	A,IYL		; FD 9D

	SBC	A,5		; DE n

	SBC	A,(HL)		; 9E

	SBC	A,(IX)		; DD 9E 00
	SBC	A,(IX+5)	; DD 9E  d
	SBC	A,(IX-5)	; DD 9E -d
	SBC	A,(IY)		; FD 9E 00
	SBC	A,(IY+5)	; FD 9E  d
	SBC	A,(IY-5)	; FD 9E -d

;-----------------------------------------------------------------------

	SBC	HL,BC		; ED 42
	SBC	HL,DE		; ED 52
	SBC	HL,HL		; ED 62
	SBC	HL,SP		; ED 72

;-----------------------------------------------------------------------

	SCF			; 37

;-----------------------------------------------------------------------

	SET	0,A		; CB C7
	SET	0,B		; CB C0
	SET	0,C		; CB C1
	SET	0,D		; CB C2
	SET	0,E		; CB C3
	SET	0,H		; CB C4
	SET	0,L		; CB C5
	SET	0,(HL)		; CB C6

	SET	1,A		; CB CF
	SET	1,B		; CB C8
	SET	1,C		; CB C9
	SET	1,D		; CB CA
	SET	1,E		; CB CB
	SET	1,H		; CB CC
	SET	1,L		; CB CD
	SET	1,(HL)		; CB CE

	SET	2,A		; CB D7
	SET	2,B		; CB D0
	SET	2,C		; CB D1
	SET	2,D		; CB D2
	SET	2,E		; CB D3
	SET	2,H		; CB D4
	SET	2,L		; CB D5
	SET	2,(HL)		; CB D6

	SET	3,A		; CB DF
	SET	3,B		; CB D8
	SET	3,C		; CB D9
	SET	3,D		; CB DA
	SET	3,E		; CB DB
	SET	3,H		; CB DC
	SET	3,L		; CB DD
	SET	3,(HL)		; CB DE

	SET	4,A		; CB E7
	SET	4,B		; CB E0
	SET	4,C		; CB E1
	SET	4,D		; CB E2
	SET	4,E		; CB E3
	SET	4,H		; CB E4
	SET	4,L		; CB E5
	SET	4,(HL)		; CB E6

	SET	5,A		; CB EF
	SET	5,B		; CB E8
	SET	5,C		; CB E9
	SET	5,D		; CB EA
	SET	5,E		; CB EB
	SET	5,H		; CB EC
	SET	5,L		; CB ED
	SET	5,(HL)		; CB EE

	SET	6,A		; CB F7
	SET	6,B		; CB F0
	SET	6,C		; CB F1
	SET	6,D		; CB F2
	SET	6,E		; CB F3
	SET	6,H		; CB F4
	SET	6,L		; CB F5
	SET	6,(HL)		; CB F6

	SET	7,A		; CB FF
	SET	7,B		; CB F8
	SET	7,C		; CB F9
	SET	7,D		; CB FA
	SET	7,E		; CB FB
	SET	7,H		; CB FC
	SET	7,L		; CB FD
	SET	7,(HL)		; CB FE

	SET	0,(IX)		; DD CB 00 C6
	SET	0,(IX+5)	; DD CB  d C6
	SET	0,(IX-5)	; DD CB -d C6
	SET	0,(IY)		; FD CB 00 C6
	SET	0,(IY+5)	; FD CB  d C6
	SET	0,(IY-5)	; FD CB -d C6

	SET	1,(IX)		; DD CB 00 CE
	SET	1,(IX+5)	; DD CB  d CE
	SET	1,(IX-5)	; DD CB -d CE
	SET	1,(IY)		; FD CB 00 CE
	SET	1,(IY+5)	; FD CB  d CE
	SET	1,(IY-5)	; FD CB -d CE

	SET	2,(IX)		; DD CB 00 D6
	SET	2,(IX+5)	; DD CB  d D6
	SET	2,(IX-5)	; DD CB -d D6
	SET	2,(IY)		; FD CB 00 D6
	SET	2,(IY+5)	; FD CB  d D6
	SET	2,(IY-5)	; FD CB -d D6

	SET	3,(IX)		; DD CB 00 DE
	SET	3,(IX+5)	; DD CB  d DE
	SET	3,(IX-5)	; DD CB -d DE
	SET	3,(IY)		; FD CB 00 DE
	SET	3,(IY+5)	; FD CB  d DE
	SET	3,(IY-5)	; FD CB -d DE

	SET	4,(IX)		; DD CB 00 E6
	SET	4,(IX+5)	; DD CB  d E6
	SET	4,(IX-5)	; DD CB -d E6
	SET	4,(IY)		; FD CB 00 E6
	SET	4,(IY+5)	; FD CB  d E6
	SET	4,(IY-5)	; FD CB -d E6

	SET	5,(IX)		; DD CB 00 EE
	SET	5,(IX+5)	; DD CB  d EE
	SET	5,(IX-5)	; DD CB -d EE
	SET	5,(IY)		; FD CB 00 EE
	SET	5,(IY+5)	; FD CB  d EE
	SET	5,(IY-5)	; FD CB -d EE

	SET	6,(IX)		; DD CB 00 F6
	SET	6,(IX+5)	; DD CB  d F6
	SET	6,(IX-5)	; DD CB -d F6
	SET	6,(IY)		; FD CB 00 F6
	SET	6,(IY+5)	; FD CB  d F6
	SET	6,(IY-5)	; FD CB -d F6

	SET	7,(IX)		; DD CB 00 FE
	SET	7,(IX+5)	; DD CB  d FE
	SET	7,(IX-5)	; DD CB -d FE
	SET	7,(IY)		; FD CB 00 FE
	SET	7,(IY+5)	; FD CB  d FE
	SET	7,(IY-5)	; FD CB -d FE

;-----------------------------------------------------------------------

	SLA	A		; CB 27
	SLA	B		; CB 20
	SLA	C		; CB 21
	SLA	D		; CB 22
	SLA	E		; CB 23
	SLA	H		; CB 24
	SLA	L		; CB 25

	SLA	(HL)		; CB 26

	SLA	(IX)		; DD CB 00 26
	SLA	(IX+5)		; DD CB  d 26
	SLA	(IX-5)		; DD CB -d 26
	SLA	(IY)		; FD CB 00 26
	SLA	(IY+5)		; FD CB  d 26
	SLA	(IY-5)		; FD CB -d 26

;-----------------------------------------------------------------------

	SRA	A		; CB 2F
	SRA	B		; CB 28
	SRA	C		; CB 29
	SRA	D		; CB 2A
	SRA	E		; CB 2B
	SRA	H		; CB 2C
	SRA	L		; CB 2D

	SRA	(HL)		; CB 2E

	SRA	(IX)		; DD CB 00 2E
	SRA	(IX+5)		; DD CB  d 2E
	SRA	(IX-5)		; DD CB -d 2E
	SRA	(IY)		; FD CB 00 2E
	SRA	(IY+5)		; FD CB  d 2E
	SRA	(IY-5)		; FD CB -d 2E

;-----------------------------------------------------------------------

	SRL	A		; CB 3F
	SRL	B		; CB 38
	SRL	C		; CB 39
	SRL	D		; CB 3A
	SRL	E		; CB 3B
	SRL	H		; CB 3C
	SRL	L		; CB 3D

	SRL	(HL)		; CB 3E

	SRL	(IX)		; DD CB 00 3E
	SRL	(IX+5)		; DD CB  d 3E
	SRL	(IX-5)		; DD CB -d 3E
	SRL	(IY)		; FD CB 00 3E
	SRL	(IY+5)		; FD CB  d 3E
	SRL	(IY-5)		; FD CB -d 3E

;-----------------------------------------------------------------------

	; A is optional

	SUB	A,A		; 97
	SUB	A,B		; 90
	SUB	A,C		; 91
	SUB	A,D		; 92
	SUB	A,E		; 93
	SUB	A,H		; 94
	SUB	A,L		; 95

	SUB	A,IXH		; DD 94
	SUB	A,IXL		; DD 95
	SUB	A,IYH		; FD 94
	SUB	A,IYL		; FD 95

	SUB	A,5		; D6 n

	SUB	A,(HL)		; 96

	SUB	A,(IX)		; DD 96 00
	SUB	A,(IX+5)	; DD 96  d
	SUB	A,(IX-5)	; DD 96 -d
	SUB	A,(IY)		; FD 96 00
	SUB	A,(IY+5)	; FD 96  d
	SUB	A,(IY-5)	; FD 96 -d

;-----------------------------------------------------------------------

	; A is optional

	XOR	A,A		; AF
	XOR	A,B		; A8
	XOR	A,C		; A9
	XOR	A,D		; AA
	XOR	A,E		; AB
	XOR	A,H		; AC
	XOR	A,L		; AD

	XOR	A,IXH		; DD AC
	XOR	A,IXL		; DD AD
	XOR	A,IYH		; FD AC
	XOR	A,IYL		; FD AD

	XOR	A,5		; EE n

	XOR	A,(HL)		; AE

	XOR	A,(IX)		; DD AE 00
	XOR	A,(IX+5)	; DD AE  d
	XOR	A,(IX-5)	; DD AE -d
	XOR	A,(IY)		; FD AE 00
	XOR	A,(IY+5)	; FD AE  d
	XOR	A,(IY-5)	; FD AE -d

;-----------------------------------------------------------------------

ADDR	equ	$

	END
