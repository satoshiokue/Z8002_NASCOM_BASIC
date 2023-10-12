;	Z8002 NASCOM BASIC
;	Converted source code from 8080/Z80 to Z8002
;	Assembler: Macro Assembler 1.42
;	Converted by Satoshi Okue
;	2023/10/12
;
;	EMUBASIC based on GRANT's BASIC
;	TARGET: EMUZ80
;	ASSEMBLER: ARCPIT XZ80.EXE
;
;	START UP ROUTINE
;	VERSION 1.0, 2022/02/15
;	WRITTEN by TETSUYA SUZUKI
;

	CPU	Z8002
	SUPMODE	ON

;;; Functions
low	function	x,(x & 255)
high	function	x,(x >> 8)

;	MEMORY ASIGN
ROMTOP	EQU	0000H
RAMTOP	EQU	8000H
RAMSIZ	EQU	1000H
TSTACK	EQU	80EDH
;
;	SCC REGISTER ADDRESS
SCCAD	EQU	0007H	; SCC DATA REGISTOR
SCCAC	EQU	0005H	; SCC CONTROL REGISTOR
WR4_V:	EQU	44H	; x16, Stop Bit 1, No-Parity
WR3_V:	EQU	0C0H	; 8 Bit, Enable
WR5_V:	EQU	0E2H	; DTR, 8 Bit, RTS
WR11_V:	EQU	50H	; Use BR output (for both Tx and Rx)
BRGTC:	EQU	11	; Baud Rate Generator Time Contant

	ORG	ROMTOP
;
;	Reset Vector

	DW	0000H	; Dummy
	DW	4000H	; FCW: System mode
	DW	START	; Initial PC
;
START:
	DI	VI,NVI
	LD	R15,#TSTACK
	JP	COLD
;
SCCINIT:
	LD	R1,#SCCAC
	LDA	R2,SCCTAB
	LD	R3,#(SCCTABE-SCCTAB)
	OTIRB	@R1,@R2,R3
	RET
;
SCCTAB:
	DB	00H			; Dummy
	DB	09H, 0C0H		; WR9: Force Hardware Reset
	DB	04H, WR4_V		; WR4
	DB	03H, WR3_V		; WR3
	DB	05H, WR5_V		; WR5
	DB	0BH, WR11_V		; WR11
	DB	0CH, low(BRGTC)		; WR12
	DB	0DH, high(BRGTC)	; WR13
	DB	0EH, 03H		; WR14: PCLK, Enable
	DB	03H, WR3_V|01H		; WR3: Rx Enable
	DB	05H, WR5_V|08H		; WR5: Tx Enable
SCCTABE:
	ALIGN	2
;
CONIN:
	INB	RL0,SCCAC
	ANDB	RL0,#01H
	JR	Z,CONIN
	INB	RH0,SCCAD
	RET
;
CONST:
	INB	RH0,SCCAC
	ANDB	RH0,#01H
	RET
;
CONOUT:
	INB	RL0,SCCAC
	ANDB	RL0,#04H
	JR	Z,CONOUT
	OUTB	SCCAD,RH0
	RET
;
;==================================================================================
; The updates to the original BASIC within this file are copyright Grant Searle
;
; You have permission to use this for NON COMMERCIAL USE ONLY
; If you wish to use it elsewhere, please include an acknowledgement to myself.
;
; http://searle.hostei.com/grant/index.html
;
; eMail: home.micros01@btinternet.com
;
; If the above don't work, please perform an Internet search to see if I have
; updated the web page hosting service.
;
;==================================================================================
;
; NASCOM ROM BASIC Ver 4.7, (C) 1978 Microsoft
; Scanned from source published in 80-BUS NEWS from Vol 2, Issue 3
; (May-June 1983) to Vol 3, Issue 3 (May-June 1984)
; Adapted for the freeware Zilog Macro Assembler 2.10 to produce
; the original ROM code (checksum A934H). PA
;
; GENERAL EQUATES
;
CTRLC	EQU	03H		; Control "C"
CTRLG	EQU	07H		; Control "G"
BKSP	EQU	08H		; Back space
LF	EQU	0AH		; Line feed
CS	EQU	0CH		; Clear screen
CR	EQU	0DH		; Carriage return
CTRLO	EQU	0FH		; Control "O"
CTRLQ	EQU	11H		; Control "Q"
CTRLR	EQU	12H		; Control "R"
CTRLS	EQU	13H		; Control "S"
CTRLU	EQU	15H		; Control "U"
ESC	EQU	1BH		; Escape
DEL	EQU	7FH		; Delete
;
; BASIC WORK SPACE LOCATIONS
;
WRKSPC	EQU	8046H		; BASIC Work space
USR	EQU	WRKSPC+04H	; "USR (x)" jump
OUTSUB	EQU	WRKSPC+08H	; "OUT p,n"
OTPORT	EQU	WRKSPC+0BH	; Port (p)
DIVSUP	EQU	WRKSPC+0EH	; Division support routine
DIV1	EQU	WRKSPC+0FH	; <- Values
DIV2	EQU	WRKSPC+17H	; <- to
DIV3	EQU	WRKSPC+1FH	; <- be
DIV4	EQU	WRKSPC+25H	; <- inserted
SEED	EQU	WRKSPC+28H	; Random number seed
LSTRND	EQU	WRKSPC+4CH	; Last random number
INPSUB	EQU	WRKSPC+50H	; #INP (x)" Routine
INPORT	EQU	WRKSPC+53H	; PORT (x)
NULLS	EQU	WRKSPC+56H	; b, Number of nulls
LWIDTH	EQU	WRKSPC+57H	; b, Terminal width
COMMAN	EQU	WRKSPC+58H	; b, Width for commas
NULFLG	EQU	WRKSPC+59H	; b, Null after input byte flag
CTLOFG	EQU	WRKSPC+5AH	; b, Control "O" flag
LINESC	EQU	WRKSPC+5CH	; w, Lines counter
LINESN	EQU	WRKSPC+5EH	; w, Lines number
CHKSUM	EQU	WRKSPC+60H	; w, Array load/save check sum
NMIFLG	EQU	WRKSPC+62H	; b, Flag for NMI break routine
BRKFLG	EQU	WRKSPC+63H	; b, Break flag
RINPUT	EQU	WRKSPC+64H	; Input reflection
POINT	EQU	WRKSPC+68H	; "POINT" reflection (unused)
PSET	EQU	WRKSPC+6CH	; "SET"	reflection
RESET	EQU	WRKSPC+70H	; "RESET" reflection
STRSPC	EQU	WRKSPC+74H	; w, Bottom of string space
LINEAT	EQU	WRKSPC+76H	; w, Current line number
BASTXT	EQU	WRKSPC+78H	; w, Pointer to start of program
BUFFER	EQU	WRKSPC+7AH	; 6 byte Input buffer
STACK	EQU	WRKSPC+80H	; Initial stack
CURPOS	EQU	WRKSPC+0C4H	; b, Character position on line

LCRFLG	EQU	WRKSPC+0C5H	; b, Locate/Create flag
TYPE	EQU	WRKSPC+0C6H	; b, Data type flag
DATFLG	EQU	WRKSPC+0C7H	; b, Literal statement flag

LSTRAM	EQU	WRKSPC+0C8H	; w, Last available RAM
TMSTPT	EQU	WRKSPC+0CAH	; w, Temporary string pointer

TMSTPL	EQU	WRKSPC+0CCH	; 12 bytes Temporary string pool
TMPSTR	EQU	WRKSPC+0D8H	; 4 bytes  Temporary string
STRBOT	EQU	WRKSPC+0DCH	; w, Bottom of string space
CUROPR	EQU	WRKSPC+0DEH	; w, Current operator in EVAL
LOOPST	EQU	WRKSPC+0E0H	; w, First statement of loop
DATLIN	EQU	WRKSPC+0E2H	; w, Line of current DATA item
FORFLG	EQU	WRKSPC+0E4H	; b, "FOR" loop flag
LSTBIN	EQU	WRKSPC+0E5H	; b, Last byte entered
READFG	EQU	WRKSPC+0E6H	; b, Read/Input flag
BRKLIN	EQU	WRKSPC+0E8H	; w, Line of break
NXTOPR	EQU	WRKSPC+0EAH	; w, Next operator in EVAL
ERRLIN	EQU	WRKSPC+0ECH	; w, Line of error
CONTAD	EQU	WRKSPC+0EEH	; w, Where to CONTinue
PROGND	EQU	WRKSPC+0F0H	; w, End of program
VAREND	EQU	WRKSPC+0F2H	; w, End of variables
ARREND	EQU	WRKSPC+0F4H	; w, End of arrays
NXTDAT	EQU	WRKSPC+0F6H	; w, Next data item
FNRGNM	EQU	WRKSPC+0F8H	; w, Name of FN argument
FNARG	EQU	WRKSPC+0FAH	; 4 byte w access / FN argument value
FPREG	EQU	WRKSPC+0FEH	; Floating point register
FPEXP	EQU	FPREG+3		; b, Floating point exponent
SGNRES	EQU	WRKSPC+102H	; b, Sign of result
PBUFF	EQU	WRKSPC+104H	; 14 byte Number print buffer
MULVAL	EQU	WRKSPC+113H	; 3 byte b+w / Multiplier
PROGST	EQU	WRKSPC+116H	; w, Start of program text area
STLOOK	EQU	WRKSPC+17AH	; Start of memory test
;
; BASIC ERROR CODE VALUES
;
NF	EQU	00H		; NEXT without FOR
SN	EQU	02H		; Syntax error
RG	EQU	04H		; RETURN without GOSUB
OD	EQU	06H		; Out of DATA
FC	EQU	08H		; Function call error
OV	EQU	0AH		; Overflow
OM	EQU	0CH		; Out of memory
UL	EQU	0EH		; Undefined line number
BS	EQU	10H		; Bad subscript
RD	EQU	12H		; Re-DIMensioned array
DZ	EQU	14H		; Division by zero (/0)
ID	EQU	16H		; Illegal direct
TM	EQU	18H		; Type miss-match
OS	EQU	1AH		; Out of string space
LS	EQU	1CH		; String too long
ST	EQU	1EH		; String formula too complex
CN	EQU	20H		; Can't CONTinue
UF	EQU	22H		; UnDEFined FN function
MO	EQU	24H		; Missing operand
HX	EQU	26H		; HEX error
BN	EQU	28H		; BIN error
;
COLD:
	JP	STARTB		; Jump for cold start
WARM:
	JP	WARMST		; Jump for warm start
STARTB:
	JP	CSTART		; Jump to initialise
;
	DW	DEINT		; Get integer -32768 to 32767
	DW	ABPASS		; Return integer in AB
;
CSTART:
	LD	R3,#WRKSPC	; Start of workspace RAM
	LD	R15,R3		; Set up a temporary stack
	JP	INITST		; Go to initialise
;
INIT:
	CALL	SCCINIT		; Initialise SCC
	LD	R2,#INITAB	; Initialise workspace
	LDB	RH1,#INITBE-INITAB+3; Bytes to copy
	LD	R3,#WRKSPC	; Into workspace RAM
COPY:
	LDB	RH0,@R2		; Get source
	LDB	@R3,RH0		; To destination
	INC	R3		; Next destination
	INC	R2		; Next source
	DECB	RH1,#1		; Count bytes
	JP	NZ,COPY		; More to move
	LD	R15,R3		; Temporary stack
	CALL	CLREG		; Clear registers and stack
	CALL	PRCRLF		; Output CRLF
;	LDB	(BUFFER+72+1),RH0	; Mark end of buffer
	LDB	(CURPOS-1),RH0	; Mark end of buffer
	LDB	(PROGST),RH0	; Initialise program area
MSIZE:
	LD	R3,#STLOOK	; Point to start of RAM
MLOOP:
	LDCTLB	RL4,FLAGS
	INC	R3		; Next byte
	LDCTLB	FLAGS,RL4
	LDB	RH0,RH3		; Above address FFFF ?
	ORB	RH0,RL3
	RESFLG	C
	JP	Z,SETTOP	; Yes - 64K RAM
	LDB	RH0,@R3		; Get contents
	LDB	RH1,RH0		; Save it
	LDCTLB	RL4,FLAGS
	COMB	RH0		; Flip all bits
	LDCTLB	FLAGS,RL4
	LDB	@R3,RH0		; Put it back
	CPB	RH0,@R3		; RAM there if same
	LDB	@R3,RH1		; Restore old contents
	JP	Z,MLOOP		; If RAM - test next byte
;
SETTOP:
	LDCTLB	RL4,FLAGS
;	DEC	R3,#1		; Back one byte
	DEC	R3,#2		; Back two byte
	LDCTLB	FLAGS,RL4
	LD	R2,#STLOOK-1	; See if enough RAM
	CALL	CPDEHL		; Compare DE with HL
	JP	C,NEMEM		; If not enough RAM
	LD	R2,#0-50	; 50 Bytes string space
	LD	(LSTRAM),R3	; Save last available RAM
	ADD	R3,R2		; Allocate string space
	LD	(STRSPC),R3	; Save string space
	CALL	CLRPTR		; Clear program area
	LD	R3,(STRSPC)	; Get end of memory
	LD	R2,#0-17	; Offset for free bytes
	ADD	R3,R2		; Adjust HL
	LD	R2,#PROGST	; Start of program text
	LDB	RH0,RL3		; Get LSB
	SUBB	RH0,RL2		; Adjust it
	LDB	RL3,RH0		; Re-save
	LDB	RH0,RH3		; Get MSB
	SBCB	RH0,RH2		; Adjust it
	LDB	RH3,RH0		; Re-save
	PUSH	@R15,R3		; Save bytes free
	LD	R3,#SIGNON	; Sign-on message
	CALL	PRS		; Output string
	POP	R3,@R15		; Get bytes free back
	CALL	PRNTHL		; Output amount of free memory
	LD	R3,#BFREE	; " Bytes free" message
	CALL	PRS		; Output string
;
WARMST:
	LD	R15,#STACK	; Temporary stack
BRKRET:
	CALL	CLREG		; Clear registers and stack
	JP	PRNTOK		; Go to get command line
;
NEMEM:
	LD	R3,#MEMMSG	; Memory size not enough
	CALL	PRS		; Print it
XXXXX:
	JP	XXXXX		; Stop
;
BFREE:
	DB	" Bytes free",CR,LF,0,0
;
SIGNON:
	DB	"Z80 Based Z8002 BASIC Ver 4.7b",CR,LF
	DB	"Copyright ",40,"C",41
	DB	" 1978 by Microsoft",CR,LF,0,0
;
MEMMSG:
	DB	"Memory size not enough",CR,LF
	DB	"The system is stopped.",CR,LF,0,0

	ALIGN	2
;
; FUNCTION ADDRESS TABLE
;
FNCTAB:
	DW	SGN
	DW	INT
	DW	ABS
	DW	USR
	DW	FRE
	DW	INP
	DW	POS
	DW	SQR
	DW	RND
	DW	LOG
	DW	EXP
	DW	COS
	DW	SIN
	DW	TAN
	DW	ATN
	DW	PEEK
	DW	DEEK
	DW	POINT
	DW	LEN
	DW	STR
	DW	VAL
	DW	ASC
	DW	CHR
	DW	HEX
	DW	BIN
	DW	LEFT
	DW	RIGHT
	DW	MID
;
; RESERVED WORD LIST
;
	ALIGN	2
WORDS:
	DB	0C5H,"ND"	; END
	DB	0C6H,"OR"	; FOR
	DB	0CEH,"EXT"	; NEXT
	DB	0C4H,"ATA"	; DATA
	DB	0C9H,"NPUT"	; INPUT
	DB	0C4H,"IM"	; DIM
	DB	0D2H,"EAD"	; READ
	DB	0CCH,"ET"	; LET
	DB	0C7H,"OTO"	; GOTO
	DB	0D2H,"UN"	; RUN
	DB	0C9H,"F"	; IF
	DB	0D2H,"ESTORE"	; RESTORE
	DB	0C7H,"OSUB"	; GOSUB
	DB	0D2H,"ETURN"	; RETURN
	DB	0D2H,"EM"	; REM
	DB	0D3H,"TOP"	; STOP
	DB	0CFH,"UT"	; OUT
	DB	0CFH,"N"	; ON
	DB	0CEH,"ULL"	; NULL
	DB	0D7H,"AIT"	; WAIT
	DB	0C4H,"EF"	; DEF
	DB	0D0H,"OKE"	; POKE
	DB	0C4H,"OKE"	; DOKE
	DB	0D3H,"CREEN"	; SCREEN
	DB	0CCH,"INES"	; LINES
	DB	0C3H,"LS"	; CLS
	DB	0D7H,"IDTH"	; WIDTH
	DB	0CDH,"ONITOR"	; MONITOR
	DB	0D3H,"ET"	; SET
	DB	0D2H,"ESET"	; RESET
	DB	0D0H,"RINT"	; PRINT
	DB	0C3H,"ONT"	; CONT
	DB	0CCH,"IST"	; LIST
	DB	0C3H,"LEAR"	; CLEAR
	DB	0C3H,"LOAD"	; CLOAD
	DB	0C3H,"SAVE"	; CSAVE
	DB	0CEH,"EW"	; NEW
;
	DB	0D4H,"AB("	; TAB(
	DB	0D4H,"O"	; TO
	DB	0C6H,"N"	; FN
	DB	0D3H,"PC("	; SPC(
	DB	0D4H,"HEN"	; THEN
	DB	0CEH,"OT"	; NOT
	DB	0D3H,"TEP"	; TEP
;
	DB	0ABH
	DB	0ADH
	DB	0AAH
	DB	0AFH
	DB	0DEH
	DB	0C1H,"ND"	; AND
	DB	0CFH,"R"	; OR
	DB	0BEH
	DB	0BDH
	DB	0BCH
;
	DB	0D3H,"GN"	; SGN
	DB	0C9H,"NT"	; INT
	DB	0C1H,"BS"	; ABS
	DB	0D5H,"SR"	; USR
	DB	0C6H,"RE"	; FRE
	DB	0C9H,"NP"	; INP
	DB	0D0H,"OS"	; POS
	DB	0D3H,"QR"	; SQR
	DB	0D2H,"ND"	; RND
	DB	0CCH,"OG"	; LOG
	DB	0C5H,"XP"	; EXP
	DB	0C3H,"OS"	; COS
	DB	0D3H,"IN"	; SIN
	DB	0D4H,"AN"	; TAN
	DB	0C1H,"TN"	; ATN
	DB	0D0H,"EEK"	; PEEK
	DB	0C4H,"EEK"	; DEEK
	DB	0D0H,"OINT"	; POINT
	DB	0CCH,"EN"	; LEN
	DB	0D3H,"TR$"	; STR
	DB	0D6H,"AL"	; VAL
	DB	0C1H,"SC"	; ASC
	DB	0C3H,"HR$"	; CHR$
	DB	0C8H,"EX$"	; HEX$
	DB	0C2H,"IN$"	; BIN$
	DB	0CCH,"EFT$"	; LEFT$
	DB	0D2H,"IGHT$"	; RIGHT$
	DB	0CDH,"ID$"	; MID$
	DB	80H		; End of list marker
;
; KEYWORD ADDRESS TABLE
;
	ALIGN	2
WORDTB:
	DW	PEND
	DW	FOR
	DW	NEXT
	DW	DATA
	DW	INPUT
	DW	DIM
	DW	READ
	DW	LET
	DW	GOTO
	DW	RUN
	DW	IF
	DW	RESTOR
	DW	GOSUB
	DW	RETURN
	DW	REM
	DW	STOP
	DW	POUT
	DW	ON
	DW	NULL
	DW	WAIT
	DW	DEF
	DW	POKE
	DW	DOKE
	DW	REM
	DW	LINES
	DW	CLS
	DW	WIDTH
	DW	MONITR
	DW	PSET
	DW	RESET
	DW	PRINT
	DW	CONT
	DW	LIST
	DW	CLEAR
	DW	REM
	DW	REM
	DW	NEW
;
; RESERVED WORD TOKEN VALUES
;
ZEND	EQU	080H		; END
ZFOR	EQU	081H		; FOR
ZDATA	EQU	083H		; DATA
ZGOTO	EQU	088H		; GOTO
ZGOSUB	EQU	08CH		; GOSUB
ZREM	EQU	08EH		; REM
ZPRINT	EQU	09EH		; PRINT
ZNEW	EQU	0A4H		; NEW
;
ZTAB	EQU	0A5H		; TAB
ZTO	EQU	0A6H		; TO
ZFN	EQU	0A7H		; FN
ZSPC	EQU	0A8H		; SPC
ZTHEN	EQU	0A9H		; THEN
ZNOT	EQU	0AAH		; NOT
ZSTEP	EQU	0ABH		; STEP
;
ZPLUS	EQU	0ACH		; +
ZMINUS	EQU	0ADH		; -
ZTIMES	EQU	0AEH		; *
ZDIV	EQU	0AFH		; /
ZOR	EQU	0B2H		; OR
ZGTR	EQU	0B3H		; >
ZEQUAL	EQU	0B4H		; M
ZLTH	EQU	0B5H		; <
ZSGN	EQU	0B6H		; SGN
ZPOINT	EQU	0C7H		; POINT
ZLEFT	EQU	0CDH +2		; LEFT$
;
; ARITHMETIC PRECEDENCE TABLE
;
	ALIGN	2
PRITAB:
	DB	79H,0		; Precedence value
	DW	PADD		; FPREG = <last> + FPREG
;
	DB	79H,0		; Precedence value
	DW	PSUB		; FPREG = <last> - FPREG
;
	DB	7CH,0		; Precedence value
	DW	MULT		; PPREG = <last> * FPREG
;
	DB	7CH,0		; Precedence value
	DW	DIV		; FPREG = <last> / FPREG
;
	DB	7FH,0		; Precedence value
	DW	POWER		; FPREG = <last> ^ FPREG
;
	DB	50H,0		; Precedence value
	DW	PAND		; FPREG = <last> AND FPREG
;
	DB	46H,0		; Precedence value
	DW	POR		; FPREG = <last> OR FPREG
;
; BASIC ERROR CODE LIST
;
	ALIGN	2
ERRORS:
	DB	"NF"		; NEXT without FOR
	DB	"SN"		; Syntax error
	DB	"RG"		; RETURN without GOSUB
	DB	"OD"		; Out of DATA
	DB	"FC"		; Illegal function call
	DB	"OV"		; Overflow error
	DB	"OM"		; Out of memory
	DB	"UL"		; Undefined line
	DB	"BS"		; Bad subscript
	DB	"DD"		; Re-DIMensioned array
	DB	"/0"		; Division by zero
	DB	"ID"		; Illegal direct
	DB	"TM"		; Type mis-match
	DB	"OS"		; Out of string space
	DB	"LS"		; String too long
	DB	"ST"		; String formula too complex
	DB	"CN"		; Can't CONTinue
	DB	"UF"		; Undefined FN function
	DB	"MO"		; Missing operand
	DB	"HX"		; HEX error
	DB	"BN"		; BIN error
;
; INITIALISATION TABLE -------------------------------------------------------
;
	ALIGN	2
INITAB:
	JP	WARMST		; Warm start jump
	JP	FCERR		; "USR (X)" jump (Set to Error)
	OUTB	(255),RH0	; "OUT p,n" skeleton
	RET
	LDB	RL4,#1		; Division support routine
	SUBB	RH0,RL4
	LDB	RL3,RH0
	LDB	RH0,RH3
	LDB	RL4,#2
	SBCB	RH0,RL4
	LDB	RH3,RH0
	LDB	RH0,RH1
	LDB	RL4,#3
	SBCB	RH0,RL4
	LDB	RH1,RH0
	LDB	RH0,#4
	RET
	DB	0,0,0			; Random number seed table used by RND
	DB	035H,04AH,0CAH,099H	;-2.65145E+07
	DB	039H,01CH,076H,098H	; 1.61291E+07
	DB	022H,095H,0B3H,098H	;-1.17691E+07
	DB	00AH,0DDH,047H,098H	; 1.30983E+07
	DB	053H,0D1H,099H,099H	;-2-01612E+07
	DB	00AH,01AH,09FH,098H	;-1.04269E+07
	DB	065H,0BCH,0CDH,098H	;-1.34831E+07
	DB	0D6H,077H,03EH,098H	; 1.24825E+07
	DB	052H,0C7H,04FH,080H	; Last random number

	ALIGN	2
	INB	RH0,(255)	; INP (x) skeleton
	RET
	DB	1		; POS (x) number (1)
	DB	255		; Terminal width (255 = no auto CRLF)
	DB	28		; Width for commas (3 columns)
	DB	0		; No nulls after input bytes
	DB	0		; Output enabled (^O off)

	ALIGN	2
	DW	20		; Initial lines counter
	DW	20		; Initial lines number
	DW	0		; Array load/save check sum
	DB	0		; Break not by NMI
	DB	0		; Break flag

	ALIGN	2

	JP	TTYLIN		; Input reflection (set to TTY)
	JP	0000H		; POINT reflection unused
	JP	0000H		; SET reflection
	JP	0000H		; RESET reflection
	DW	STLOOK		; Temp string space
	DW	-2		; Current line number (cold)
	DW	PROGST+1	; Start of program text
INITBE:
;
; END OF INITIALISATION TABLE ---------------------------------------------------
;
ERRMSG:
	DB	" Error",0
INMSG:
	DB	" in ",0
ZERBYT	EQU	$-1		; A zero byte
OKMSG:
	DB	"Ok",CR,LF,0,0
BRKMSG:
	DB	"Break",0
;
	ALIGN	2

BAKSTK:
	LD	R3,#4		; Look for "FOR" block with
	ADD	R3,R15		; same index as specified
LOKFOR:
	LDB	RH0,@R3		; Get block ID
	INC	R3		; Point to index address
	INC	R3		; Point to index address
	CPB	RH0,#ZFOR	; Is it a "FOR" token
	RET	NZ		; No - exit
	LDB	RL1,@R3		; BC = Address of "FOR" index
	INC	R3
	LDB	RH1,@R3
	INC	R3		; Point to sign of STEP
	PUSH	@R15,R3		; Save pointer to sign
	LDB	RL3,RL1		; HL = address of "FOR" index
	LDB	RH3,RH1
	LDB	RH0,RH2		; See if an index was specified
	ORB	RH0,RL2		; DE = 0 if no index specified
	EX	R3,R2		; Specified index into HL
	JP	Z,INDFND	; Skip if no index given
	EX	R3,R2		; Index back into DE
	CALL	CPDEHL		; Compare index with one given
INDFND:
;	LD	R1,#16-3	; Offset to next block : 8 levels *2 byte
	LD	R1,#18-4	; Offset to next block : 9 levels *2 byte
	POP	R3,@R15		; Restore pointer to sign
	RET	Z		; Return if block found
	ADD	R3,R1		; Point to next block
	JP	LOKFOR		; Keep on looking
;
MOVUP:
	CALL	ENFMEM		; See if enough memory
MOVSTR:
	PUSH	@R15,R1		; Save end of source
	EX	@R15,R3		; Swap source and dest" end
	POP	R1,@R15		; Get end of destination
MOVLP:
	CALL	CPDEHL		; See if list moved
	LDB	RH0,@R3		; Get byte
	LDB	@R1,RH0		; Move it
	RET	Z		; Exit if all done
	LDCTLB	RL4,FLAGS
	DEC	R1,#1		; Next byte to move to
	DEC	R3,#1		; Next byte to move
	LDCTLB	FLAGS,RL4
	JP	MOVLP		; Loop until all bytes moved
;
CHKSTK:
	PUSH	@R15,R3		; Save code string address
	LD	R3,(ARREND)	; Lowest free memory
	LDB	RH1,#0		; BC = Number of levels to test
	ADD	R3,R1		; 2 Bytes for each level
	ADD	R3,R1
	JR	ENFMEM1		; Skip "PUSH HL"
ENFMEM:
	PUSH	@R15,R3		; Save code string address
ENFMEM1:
	LDB	RH0,#0D0H	; LOW -48; 48 Bytes minimum RAM
	SUBB	RH0,RL3
	LDB	RL3,RH0
	LDB	RH0,#0FFH	; HIGH (-48); 48 Bytes minimum RAM
	SBCB	RH0,RH3
	JP	C,OMERR		; Not enough - ?OM Error
	LDB	RH3,RH0
	ADD	R3,R15		; Test if stack is overflowed
	POP	R3,@R15		; Restore code string address
	RET	C		; Return if enough mmory
OMERR:
	LDB	RL2,#OM		; ?OM Error
	JP	ERROR
;
DATSNR:
	LD	R3,(DATLIN)	; Get line of current DATA item
	LD	(LINEAT),R3	; Save as current line
SNERR:
	LDB	RL2,#SN		; ?SN Error
	JR	ERROR
DZERR:
	LDB	RL2,#DZ		; ?/0 Error
	JR	ERROR
NFERR:
	LDB	RL2,#NF		; ?NF Error
	JR	ERROR
DDERR:
	LDB	RL2,#RD		; ?DD Error
	JR	ERROR
UFERR:
	LDB	RL2,#UF		; ?UF Error
	JR	ERROR
OVERR:
	LDB	RL2,#OV		; ?OV Error
	JR	ERROR
TMERR:
	LDB	RL2,#TM		; ?TM Error
;
ERROR:
	CALL	CLREG		; Clear registers and stack
	LDB	(CTLOFG),RH0	; Enable output (A is 0)
	CALL	STTLIN		; Start new line
	LD	R3,#ERRORS	; Point to error codes
	LDB	RH2,RH0		; D = 0 (A is 0)
	LDB	RH0,#'?'
	CALL	OUTC		; Output '?'
	ADD	R3,R2		; Offset to correct error code
	LDB	RH0,@R3		; First character
	CALL	OUTC		; Output it
	CALL	GETCHR		; Get next character
	CALL	OUTC		; Output it
	LD	R3,#ERRMSG	; "Error" message
ERRIN:
	CALL	PRS		; Output message
	LD	R3,(LINEAT)	; Get line of error
	LD	R2,#-2		; Cold start error if -2
	CALL	CPDEHL		; See if cold start error
	JP	Z,CSTART	; Cold start error - Restart
	LDB	RH0,RH3		; Was it a direct error?
	ANDB	RH0,RL3
	RESFLG	C		; Line = -1 if direct error
	INCB	RH0,#1
	JR	Z,ERRIN1
	CALL	LINEIN		; No - output line of error
ERRIN1:
	JR	PRNTOK		; Skip "POP BC"
POPNOK:
	POP	R1,@R15		; Drop address in input buffer
;
PRNTOK:
	XORB	RH0,RH0
	RESFLG	C		; Output "Ok" and get command
	LDB	(CTLOFG),RH0	; Enable output
	CALL	STTLIN		; Start new line
	LD	R3,#OKMSG	; "Ok" message
	CALL	PRS		; Output "Ok"
GETCMD:
	LD	R3,#-1		; Flag direct mode
	LD	(LINEAT),R3	; Save as current line
	CALL	GETLIN		; Get an input line
	JP	C,GETCMD	; Get line again if break
	CALL	GETCHR		; Get first character
	INCB	RH0,#1		; Test if end of line
	DECB	RH0,#1		; Without affecting Carry
	JP	Z,GETCMD	; Nothing entered - Get another
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Save Carry status
	CALL	ATOH		; Get line number into DE
	PUSH	@R15,R2		; Save line number
	CALL	CRUNCH		; Tokenise rest of line
	LDB	RH1,RH0		; Length of tokenised line
	POP	R2,@R15		; Restore line number
	POP	R0,@R15		; Restore Carry
	LDCTLB	FLAGS,RL0
	JP	NC,EXCUTE	; No line number - Direct mode
	PUSH	@R15,R2		; Save line number
	PUSH	@R15,R1		; Save length of tokenised line
	XORB	RH0,RH0
	RESFLG	C
	LDB	(LSTBIN),RH0	; Clear last byte input
	CALL	GETCHR		; Get next character
	ORB	RH0,RH0
	RESFLG	C		; Set flags
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; And save them
	CALL	SRCHLN		; Search for line number in DE
	JP	C,LINFND	; Jump if line found
	POP	R0,@R15		; Get status
	LDCTLB	FLAGS,RL0
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; And re-save
	JP	Z,ULERR		; Nothing after number - Error
	ORB	RH0,RH0
	RESFLG	C		; Clear Carry
LINFND:
	PUSH	@R15,R1		; Save address of line in prog
	JP	NC,INEWLN	; Line not found - Insert new
	EX	R3,R2		; Next line address in DE
	LD	R3,(PROGND)	; End of program
SFTPRG:
	LDB	RH0,@R2		; Shift rest of program down
	LDB	@R1,RH0
	LDCTLB	RL4,FLAGS
	INC	R1		; Next destination
	INC	R2		; Next source
	LDCTLB	FLAGS,RL4
	CALL	CPDEHL		; All done?
	JP	NZ,SFTPRG	; More to do
	LDB	RH3,RH1		; HL - New end of program
	LDB	RL3,RL1
	LD	(PROGND),R3	; Update end of program
;
INEWLN:
	POP	R2,@R15		; Get address of line,
	POP	R0,@R15		; Get status
	LDCTLB	FLAGS,RL0
	JP	Z,SETPTR	; No text - Set up pointers
	LD	R3,(PROGND)	; Get end of program
	EX	@R15,R3		; Get length of input line
	POP	R1,@R15		; End of program to BC
	ADD	R3,R1		; Find new end
	PUSH	@R15,R3		; Save new end
	CALL	MOVUP		; Make space for line
	POP	R3,@R15		; Restore new end
	LD	(PROGND),R3	; Update end of program pointer
	EX	R3,R2		; Get line to move up in HL
	LDB	@R3,RH3		; Save MSB
	POP	R2,@R15		; Get new line number
	LDCTLB	RL4,FLAGS
	INC	R3		; Skip pointer
	INC	R3
	LDB	@R3,RL2		; Save LSB of line number
	INC	R3
	LDB	@R3,RH2		; Save MSB of line number
	INC	R3		; To first byte in line
	LDCTLB	FLAGS,RL4
	LD	R2,#BUFFER	; Copy buffer to program
MOVBUF:
	LDB	RH0,@R2		; Get source
	LDB	@R3,RH0		; Save destinations
	LDCTLB	RL4,FLAGS
	INC	R3		; Next source
	INC	R2		; Next destination
	LDCTLB	FLAGS,RL4
	ORB	RH0,RH0
	RESFLG	C		; Done?
	JP	NZ,MOVBUF	; No - Repeat
SETPTR:
	CALL	RUNFST		; Set line pointers
	LDCTLB	RL4,FLAGS
	INC	R3		; To LSB of pointer
	LDCTLB	FLAGS,RL4
	EX	R3,R2		; Address to DE
PTRLP:
	LDB	RH3,RH2		; Address to HL
	LDB	RL3,RL2
	LDB	RH0,@R3		; Get LSB of pointer
	LDCTLB	RL4,FLAGS
	INC	R3		; To MSB of pointer
	LDCTLB	FLAGS,RL4
	ORB	RH0,@R3
	RESFLG	C		; Compare with MSB pointer
	JP	Z,GETCMD	; Get command line if end
	LDCTLB	RL4,FLAGS
	INC	R3		; To LSB of line number
	INC	R3		; Skip line number
	INC	R3		; Point to first byte in line
	LDCTLB	FLAGS,RL4
	XORB	RH0,RH0
	RESFLG	C		; Looking for 00 byte
FNDEND:
	CPB	RH0,@R3		; Found end of line?
	LDCTLB	RL4,FLAGS
	INC	R3		; Move to next byte
	LDCTLB	FLAGS,RL4
	JP	NZ,FNDEND	; No - Keep looking
	EX	R3,R2		; Next line address to HL
	LDB	@R3,RL2		; Save LSB of pointer
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	@R3,RH2		; Save MSB of pointer
	JP	PTRLP		; Do next line
;
SRCHLN:
	LD	R3,(BASTXT)	; Start of program text
SRCHLP:
	LDB	RH1,RH3		; BC = Address to look at
	LDB	RL1,RL3
	LDB	RH0,@R3		; Get address of next line
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	ORB	RH0,@R3
	RESFLG	C		; End of program found?
	LDCTLB	RL4,FLAGS
	DEC	R3,#1
	LDCTLB	FLAGS,RL4
	RET	Z		; Yes - Line not found
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	RH0,@R3		; Get LSB of line number
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	RH3,@R3		; Get MSB of line number
	LDB	RL3,RH0
	CALL	CPDEHL		; Compare with line in DE
	LDB	RH3,RH1		; HL = Start of this line
	LDB	RL3,RL1
	LDB	RH0,@R3		; Get LSB of next line address
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	RH3,@R3		; Get MSB of next line address
	LDB	RL3,RH0		; Next line to HL
	COMFLG	C
	RET	Z		; Lines found - Exit
	COMFLG	C
	RET	NC		; Line not found,at line after
	JP	SRCHLP		; Keep looking
;
NEW:
	RET	NZ		; Return if any more on line
CLRPTR:
	LD	R3,(BASTXT)	; Point to start of program
	XORB	RH0,RH0
	RESFLG	C		; Set program area to empty
	LDB	@R3,RH0		; Save LSB = 00
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	@R3,RH0		; Save MSB = 00
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	LD	(PROGND),R3	; Set program end
;
RUNFST:
	LD	R3,(BASTXT)	; Clear all variables
	LDCTLB	RL4,FLAGS
	DEC	R3,#1
	LDCTLB	FLAGS,RL4
;
INTVAR:
	LD	(BRKLIN),R3	; Initialise RUN variables
	LD	R3,(LSTRAM)	; Get end of RAM
	LD	(STRBOT),R3	; Clear string space
	XORB	RH0,RH0
	RESFLG	C
	CALL	RESTOR		; Reset DATA pointers
	LD	R3,(PROGND)	; Get end of program
	LD	(VAREND),R3	; Clear variables
	LD	(ARREND),R3	; Clear arrays
;
CLREG:
	POP	R1,@R15		; Save return address
	LD	R3,(STRSPC)	; Get end of working RAN
	LD	R15,R3		; Set stack
	LD	R3,#TMSTPL	; Temporary string pool
	LD	(TMSTPT),R3	; Reset temporary string ptr
	XORB	RH0,RH0
	RESFLG	C		; A = 00
	LDB	RL3,RH0		; HL = 0000
	LDB	RH3,RH0
	LD	(CONTAD),R3	; No CONTinue
	LDB	(FORFLG),RH0	; Clear FOR flag
	LD	(FNRGNM),R3	; Clear FN argument
	PUSH	@R15,R3		; HL = 0000
	PUSH	@R15,R1		; Put back return
DOAGN:
	LD	R3,(BRKLIN)	; Get address of code to RUN
	RET			; Return to execution driver
;
PROMPT:
	LDB	RH0,#'?'	; '?'
	CALL	OUTC		; Output character
	LDB	RH0,#' '	; Space
	CALL	OUTC		; Output character
	JP	RINPUT		; Get input line
;
CRUNCH:
	XORB	RH0,RH0
	RESFLG	C		; Tokenise line @ HL to BUFFER
	LDB	(DATFLG),RH0	; Reset literal flag
	LDB	RL1,#2+3	; 2 byte number and 3 nulls
	LD	R2,#BUFFER	; Start of input buffer
CRNCLP:
	LDB	RH0,@R3		; Get byte
	CPB	RH0,#' '	; Is it a space?
	JP	Z,MOVDIR	; Yes - Copy direct
	LDB	RH1,RH0		; Save character
	CPB	RH0,#'"'	; Is it a quote?
	JP	Z,CPYLIT	; Yes - Copy literal string
	ORB	RH0,RH0
	RESFLG	C		; Is it end of buffer?
	JP	Z,ENDBUF	; Yes - End buffer
	LDB	RH0,(DATFLG)	; Get data type
	ORB	RH0,RH0
	RESFLG	C		; Literal?
	LDB	RH0,@R3		; Get byte to copy
	JP	NZ,MOVDIR	; Literal - Copy direct
	CPB	RH0,#'?'	; Is it '?' short for PRINT
	LDB	RH0,#ZPRINT	; "PRINT" token
	JP	Z,MOVDIR	; Yes - replace it
	LDB	RH0,@R3		; Get byte again
	CPB	RH0,#'0'	; Is it less than '0'
	JP	C,FNDWRD	; Yes - Look for reserved words
	CPB	RH0,#60		; ";"+1; Is it "0123456789:;" ?
	JP	C,MOVDIR	; Yes - copy it direct
FNDWRD:
	PUSH	@R15,R2		; Look for reserved words
	LD	R2,#WORDS-1	; Point to table
	PUSH	@R15,R1		; Save count
	LD	R1,#RETNAD	; Where to return to
	PUSH	@R15,R1		; Save return address
	LDB	RH1,#ZEND-1	; First token value -1
	LDB	RH0,@R3		; Get byte
	CPB	RH0,#'a'	; Less than 'a' ?
	JP	C,SEARCH	; Yes - search for words
	CPB	RH0,#'z'+1	; Greater than 'z' ?
	JP	NC,SEARCH	; Yes - search for words
	ANDB	RH0,#01011111B	; Force upper case
	LDB	@R3,RH0		; Replace byte
SEARCH:
	LDB	RL1,@R3		; Search for a word
	EX	R3,R2
GETNXT:
	LDCTLB	RL4,FLAGS
	INC	R3		; Get next reserved word
	LDCTLB	FLAGS,RL4
	ORB	RH0,@R3
	RESFLG	C		; Start of word?
	JP	PL,GETNXT	; No - move on
	INCB	RH1,#1		; Increment token value
	LDB	RH0,@R3		; Get byte from table
	ANDB	RH0,#01111111B	; Strip bit 7
	RET	Z		; Return if end of list
	CPB	RH0,RL1		; Same character as in buffer?
	JP	NZ,GETNXT	; No - get next word
	EX	R3,R2
	PUSH	@R15,R3		; Save start of word
;
NXTBYT:
	LDCTLB	RL4,FLAGS
	INC	R2		; Look through rest of word
	LDCTLB	FLAGS,RL4
	LDB	RH0,@R2		; Get byte from table
	ORB	RH0,RH0
	RESFLG	C		; End of word ?
	JP	MI,MATCH	; Yes - Match found
	LDB	RL1,RH0		; Save it
	LDB	RH0,RH1		; Get token value
	CPB	RH0,#ZGOTO	; Is it "GOTO" token ?
	JP	NZ,NOSPC	; No - Don't allow spaces
	CALL	GETCHR		; Get next character
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; Cancel increment from GETCHR
	LDCTLB	FLAGS,RL4
NOSPC:
	INC	R3		; Next byte
	LDB	RH0,@R3		; Get byte
	CPB	RH0,#'a'	; Less than 'a' ?
	JP	C,NOCHNG	; Yes - don't change
	ANDB	RH0,#01011111B	; Make upper case
NOCHNG:
	CPB	RH0,RL1		; Same as in buffer ?
	JP	Z,NXTBYT	; Yes - keep testing
	POP	R3,@R15		; Get back start of word
	JP	SEARCH		; Look at next word
;
MATCH:
	LDB	RL1,RH1		; Word found - Save token value
	POP	R0,@R15		; Throw away return
	LDCTLB	FLAGS,RL0
	EX	R3,R2
	RET			; Return to "RETNAD"
RETNAD:
	EX	R3,R2		; Get address in string
	LDB	RH0,RL1		; Get token value
	POP	R1,@R15		; Restore buffer length
	POP	R2,@R15		; Get destination address
MOVDIR:
	LDCTLB	RL4,FLAGS
	INC	R3		; Next source in buffer
	LDCTLB	FLAGS,RL4
	LDB	@R2,RH0		; Put byte in buffer
	LDCTLB	RL4,FLAGS
	INC	R2		; Move up buffer
	LDCTLB	FLAGS,RL4
	INCB	RL1,#1		; Increment length of buffer
	SUBB	RH0,#':'	; End of statement?
	JP	Z,SETLIT	; Jump if multi-statement line
	CPB	RH0,#ZDATA-3AH	; Is it DATA statement ?
	JP	NZ,TSTREM	; No - see if REM
SETLIT:
	LDB	(DATFLG),RH0	; Set literal flag
TSTREM:
	SUBB	RH0,#ZREM-3AH	; Is it REM?
	JP	NZ,CRNCLP	; No - Leave flag
	LDB	RH1,RH0		; Copy rest of buffer
NXTCHR:
	LDB	RH0,@R3		; Get byte
	ORB	RH0,RH0
;	RESFLG	C		; End of line ?
	JP	Z,ENDBUF	; Yes - Terminate buffer
	CPB	RH0,RH1		; End of statement ?
	JP	Z,MOVDIR	; Yes - Get next one
CPYLIT:
	LDCTLB	RL4,FLAGS
	INC	R3		; Move up source string
	LDB	@R2,RH0		; Save in destination
	INCB	RL1,#1		; Increment length
	INC	R2		; Move up destination
	LDCTLB	FLAGS,RL4
	JP	NXTCHR		; Repeat
;
ENDBUF:
	LD	R3,#BUFFER-1	; Point to start of buffer
	LDB	@R2,RH0		; Mark end of buffer (A = 00)
	LDCTLB	RL4,FLAGS
	INC	R2
	LDCTLB	FLAGS,RL4
	LDB	@R2,RH0		; A = 00
	LDCTLB	RL4,FLAGS
	INC	R2
	LDCTLB	FLAGS,RL4
	LDB	@R2,RH0		; A = 00
	RET
;
DODEL:
	LDB	RH0,(NULFLG)	; Get null flag status
	ORB	RH0,RH0
	RESFLG	C		; Is it zero?
	LDB	RH0,#0		; Zero A - Leave flags
	LDB	(NULFLG),RH0	; Zero null flag
	JP	NZ,ECHDEL	; Set - Echo it
	DECB	RH1,#1		; Decrement length
	JP	Z,GETLIN	; Get line again if empty
	CALL	OUTC		; Output null character
	JR	ECHDEL1		; Skip "DEC B"
ECHDEL:
	DECB	RH1,#1		; Count bytes in buffer
ECHDEL1:
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; Back space buffer
	LDCTLB	FLAGS,RL4
	JP	Z,OTKLN		; No buffer - Try again
	LDB	RH0,@R3		; Get deleted byte
	CALL	OUTC		; Echo it
	JP	MORINP		; Get more input
;
DELCHR:
	DECB	RH1,#1		; Count bytes in buffer
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; Back space buffer
	LDCTLB	FLAGS,RL4
	CALL	OUTC		; Output character in A
	JP	NZ,MORINP	; Not end - Get more
OTKLN:
	CALL	OUTC		; Output character in A
KILIN:
	CALL	PRCRLF		; Output CRLF
	JP	TTYLIN		; Get line again
;
GETLIN:
TTYLIN:
	LD	R3,#BUFFER	; Get a line by character
	LDB	RH1,#1		; Set buffer as empty
	XORB	RH0,RH0
	RESFLG	C
	LDB	(NULFLG),RH0	; Clear null flag
MORINP:
	CALL	CLOTST		; Get character and test ^O
	LDB	RL1,RH0		; Save character in C
	CPB	RH0,#DEL	; Delete character?
	JP	Z,DODEL		; Yes - Process it
	LDB	RH0,(NULFLG)	; Get null flag
	ORB	RH0,RH0
	RESFLG	C		; Test null flag status
	JP	Z,PROCES	; Reset - Process character
	LDB	RH0,#0		; Set a null
	CALL	OUTC		; Output null
	XORB	RH0,RH0
	RESFLG	C		; Clear A
	LDB	(NULFLG),RH0	; Reset null flag
PROCES:
	LDB	RH0,RL1		; Get character
	CPB	RH0,#CTRLG	; Bell?
	JP	Z,PUTCTL	; Yes - Save it
	CPB	RH0,#CTRLC	; Is it control "C"?
	JR	NZ,PROCES1
	CALL	PRCRLF		; Yes - Output CRLF
PROCES1:
	SETFLG	C		; Flag break
	RET	Z		; Return if control "C"
	CPB	RH0,#CR		; Is it enter?
	JP	Z,ENDINP	; Yes - Terminate input
	CPB	RH0,#CTRLU	; Is it control "U"?
	JP	Z,KILIN		; Yes - Get another line
	CPB	RH0,#'@'	; Is it "kill line"?
	JP	Z,OTKLN		; Yes - Kill line
	CPB	RH0,#'_'	; Is it delete?
	JP	Z,DELCHR	; Yes - Delete character
	CPB	RH0,#BKSP	; Is it backspace?
	JP	Z,DELCHR	; Yes - Delete character
	CPB	RH0,#CTRLR	; Is it control "R"?
	JP	NZ,PUTBUF	; No - Put in buffer
	PUSH	@R15,R1		; Save buffer length
	PUSH	@R15,R2		; Save DE
	PUSH	@R15,R3		; Save buffer address
	LDB	@R3,#0		; Mark end of buffer
	CALL	OUTNCR		; Output and do CRLF
	LD	R3,#BUFFER	; Point to buffer start
	CALL	PRS		; Output buffer
	POP	R3,@R15		; Restore buffer address
	POP	R2,@R15		; Restore DE
	POP	R1,@R15		; Restore buffer length
	JP	MORINP		; Get another character
;
PUTBUF:
	CPB	RH0,#' '	; Is it a control code?
	JP	C,MORINP	; Yes - Ignore
PUTCTL:
	LDB	RH0,RH1		; Get number of bytes in buffer
	CPB	RH0,#72+1	; Test for line overflow
	LDB	RH0,#CTRLG	; Set a bell
	JP	NC,OUTNBS	; Ring bell if buffer full
	LDB	RH0,RL1		; Get character
	LDB	@R3,RL1		; Save in buffer
	LDB	(LSTBIN),RH0	; Save last input byte
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4	; Move up buffer
	INCB	RH1,#1		; Increment length
OUTIT:
	CALL	OUTC		; Output the character entered
	JP	MORINP		; Get another character
;
OUTNBS:
	CALL	OUTC		; Output bell and back over it
	LDB	RH0,#BKSP	; Set back space
	JP	OUTIT		; Output it and get more
;
CPDEHL:
	LDB	RH0,RH3		; Get H
	SUBB	RH0,RH2		; Compare with D
	RET	NZ		; Different - Exit
	LDB	RH0,RL3		; Get L
	SUBB	RH0,RL2		; Compare with E
	RET			; Return status
;
CHKSYN:
	LDB	RH0,@R3		; Check syntax of character
	CPB	RH0,RH4		; Same as in code string?
;	EX	@R15,R3		; Address of test byte
;	CPB	RH0,@R3		; Same as in code string?
;	INC	R3
;	EX	@R15,R3		; Put it back
	JP	Z,GETCHR	; Yes - Get next character
	JP	SNERR		; Different - ?SN Error
;
OUTC:
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Save character
	LDB	RH0,(CTLOFG)	; Get control "O" flag
	ORB	RH0,RH0
	RESFLG	C		; Is it set?
	JP	NZ,POPAF	; Yes - don't output
	POP	R0,@R15		; Restore character
	LDCTLB	FLAGS,RL0
	PUSH	@R15,R1		; Save buffer length
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Save character
	CPB	RH0,#' '	; Is it a control code?
	JP	C,DINPOS	; Yes - Don't INC POS(X)
	LDB	RH0,(LWIDTH)	; Get line width
	LDB	RH1,RH0		; To B
	LDB	RH0,(CURPOS)	; Get cursor position
	INCB	RH1,#1		; Width 255?
	JP	Z,INCLEN	; Yes - No width limit
	DECB	RH1,#1		; Restore width
	CPB	RH0,RH1		; At end of line?
	JR	NZ,INCLEN
	CALL	PRCRLF		; Yes - output CRLF
INCLEN:
	INCB	RH0,#1		; Move on one character
	LDB	(CURPOS),RH0	; Save new position
DINPOS:
	POP	R0,@R15		; Restore character
	LDCTLB	FLAGS,RL0
	POP	R1,@R15		; Restore buffer length
	CALL	MONOUT		; Send it
	RET
;
CLOTST:
	CALL	GETINP		; Get input character
	ANDB	RH0,#01111111B	; Strip bit 7
	CPB	RH0,#CTRLO	; Is it control "O"?
	RET	NZ		; No don't flip flag
	LDB	RH0,(CTLOFG)	; Get flag
	LDCTLB	RL4,FLAGS
	COMB	RH0			; Flip it
	LDCTLB	FLAGS,RL4
	LDB	(CTLOFG),RH0	; Put it back
	XORB	RH0,RH0
	RESFLG	C		; Null character
	RET
;
LIST:
	CALL	ATOH		; ASCII number to DE
	RET	NZ		; Return if anything extra
	POP	R1,@R15		; Rubbish - Not needed
	CALL	SRCHLN		; Search for line number in DE
	PUSH	@R15,R1		; Save address of line
	CALL	SETLIN		; Set up lines counter
LISTLP:
	POP	R3,@R15		; Restore address of line
	LDB	RL1,@R3		; Get LSB of next line
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	RH1,@R3		; Get MSB of next line
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	RH0,RH1		; BC = 0 (End of program)?
	ORB	RH0,RL1
	RESFLG	C
	JP	Z,PRNTOK	; Yes - Go to command mode
	CALL	COUNT		; Count lines
	CALL	TSTBRK		; Test for break key
	PUSH	@R15,R1		; Save address of next line
	CALL	PRCRLF		; Output CRLF
	LDB	RL2,@R3		; Get LSB of line number
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	RH2,@R3		; Get MSB of line number
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	PUSH	@R15,R3		; Save address of line start
	EX	R3,R2		; Line number to HL
	CALL	PRNTHL		; Output line number in decimal
	LDB	RH0,#' '	; Space after line number
	POP	R3,@R15		; Restore start of line address
LSTLP2:
	CALL	OUTC		; Output character in A
LSTLP3:
	LDB	RH0,@R3		; Get next byte in line
	ORB	RH0,RH0
	RESFLG	C		; End of line?
	LDCTLB	RL4,FLAGS
	INC	R3		; To next byte in line
	LDCTLB	FLAGS,RL4
	JP	Z,LISTLP	; Yes - get next line
	JP	PL,LSTLP2	; No token - output it
	SUBB	RH0,#ZEND-1	; Find and output word
	LDB	RL1,RH0		; Token offset+1 to C
	LD	R2,#WORDS	; Reserved word list
FNDTOK:
	LDB	RH0,@R2		; Get character in list
	LDCTLB	RL4,FLAGS
	INC	R2		; Move on to next
	LDCTLB	FLAGS,RL4
	ORB	RH0,RH0
	RESFLG	C		; Is it start of word?
	JP	PL,FNDTOK	; No - Keep looking for word
	DECB	RL1,#1		; Count words
	JP	NZ,FNDTOK	; Not there - keep looking
OUTWRD:
	ANDB	RH0,#01111111B	; Strip bit 7
	CALL	OUTC		; Output first character
	LDB	RH0,@R2		; Get next character
	LDCTLB	RL4,FLAGS
	INC	R2		; Move on to next
	LDCTLB	FLAGS,RL4
	ORB	RH0,RH0
	RESFLG	C		; Is it end of word?
	JP	PL,OUTWRD	; No - output the rest
	JP	LSTLP3		; Next byte in line
;
SETLIN:
	PUSH	@R15,R3		; Set up LINES counter
	LD	R3,(LINESN)	; Get LINES number
	LD	(LINESC),R3	; Save in LINES counter
	POP	R3,@R15
	RET
;
COUNT:
	PUSH	@R15,R3		; Save code string address
	PUSH	@R15,R2
	LD	R3,(LINESC)	; Get LINES counter
	LD	R2,#-1
	ADC	R3,R2		; Decrement
	LD	(LINESC),R3	; Put it back
	POP	R2,@R15
	POP	R3,@R15		; Restore code string address
	RET	PL		; Return if more lines to go
	PUSH	@R15,R3		; Save code string address
	LD	R3,(LINESN)	; Get LINES number
	LD	(LINESC),R3	; Reset LINES counter
	CALL	GETINP		; Get input character
	CPB	RH0,#CTRLC	; Is it control "C"?
	JP	Z,RSLNBK	; Yes - Reset LINES and break
	POP	R3,@R15		; Restore code string address
	JP	COUNT		; Keep on counting
;
RSLNBK:
	LD	R3,(LINESN)	; Get LINES number
	LD	(LINESC),R3	; Reset LINES counter
	JP	BRKRET		; Go and output "Break"
;
FOR:
	LDB	RH0,#64H	; Flag "FOR" assignment
	LDB	(FORFLG),RH0	; Save "FOR" flag
	CALL	LET		; Set up initial index
	POP	R1,@R15		; Drop RETurn address
	PUSH	@R15,R3		; Save code string address
	CALL	DATA		; Get next statement address
	LD	(LOOPST),R3	; Save it for start of loop
	LD	R3,#2		; Offset for "FOR" block
	ADD	R3,R15		; Point to it
FORSLP:
	CALL	LOKFOR		; Look for existing "FOR" block
	POP	R2,@R15		; Get code string address
	JP	NZ,FORFND	; No nesting found
	ADD	R3,R1		; Move into "FOR" block
	PUSH	@R15,R2		; Save code string address
	LDCTLB	RL4,FLAGS
	DEC	R3,#1
	LDB	RH2,@R3		; Get MSB of loop statement
	DEC	R3,#1
	LDB	RL2,@R3		; Get LSB of loop statement
	INC	R3
	INC	R3
	LDCTLB	FLAGS,RL4
	PUSH	@R15,R3		; Save block address
	LD	R3,(LOOPST)	; Get address of loop statement
	CALL	CPDEHL		; Compare the FOR loops
	POP	R3,@R15		; Restore block address
	JP	NZ,FORSLP	; Different FORs - Find another
	POP	R2,@R15		; Restore code string address
	LD	R15,R3		; Remove all nested loops
;
FORFND:
	EX	R3,R2		; Code string address to HL
;	LDB	RL1,#8
	LDB	RL1,#9		; @@@@
	CALL	CHKSTK		; Check for 8 levels of stack
	PUSH	@R15,R3		; Save code string address
	LD	R3,(LOOPST)	; Get first statement of loop
	EX	@R15,R3		; Save and restore code string
	PUSH	@R15,R3		; Re-save code string address
	LD	R3,(LINEAT)	; Get current line number
	EX	@R15,R3		; Save and restore code string
	CALL	TSTNUM		; Make sure it's a number
	LD	RH4,#ZTO	; "TO" token
	CALL	CHKSYN		; Make sure "TO" is next
	CALL	GETNUM		; Get "TO" expression value
	PUSH	@R15,R3		; Save code string address
	CALL	BCDEFP		; Move "TO" value to BCDE
	POP	R3,@R15		; Restore code string address
	EXB	RH1,RL1
	EXB	RH2,RL2
	PUSH	@R15,R1		; Save "TO" value in block
	PUSH	@R15,R2
	LD	R1,#8100H	; BCDE - 1 (default STEP)
	LDB	RH2,RL1		; C=0
	LDB	RL2,RH2		; D=0
	LDB	RH0,@R3		; Get next byte in code string
	CPB	RH0,#ZSTEP	; See if "STEP" is stated
	LDB	RH0,#1		; Sign of step = 1
	JP	NZ,SAVSTP	; No STEP given - Default to 1
	CALL	GETCHR		; Jump over "STEP" token
	CALL	GETNUM		; Get step value
	PUSH	@R15,R3		; Save code string address
	CALL	BCDEFP		; Move STEP to BCDE
	CALL	TSTSGN		; Test sign of FPREG
	POP	R3,@R15		; Restore code string address
SAVSTP:
	EXB	RH1,RL1
	EXB	RH2,RL2

	PUSH	@R15,R1		; Save the STEP value in block
	PUSH	@R15,R2
	PUSH	@R15,R0		; Save sign of STEP
;	INC	R15		; Don't save flags
	PUSH	@R15,R3		; Save code string address
	LD	R3,(BRKLIN)	; Get address of index variable
	EXB	RH3,RL3
	EX	@R15,R3		; Save and restore code string
PUTFID:
;	LDB	RH1,#ZFOR	; "FOR" block marker
;	PUSH	@R15,R1		; Save it
;	INC	R15		; Don't save C
	LDB	RH1,#ZFOR	; "FOR" block marker
	LDB	RL1,#0		;
	PUSH	@R15,R1		; Save it
;
RUNCNT:
	CALL	TSTBRK		; Execution driver - Test break
	LD	(BRKLIN),R3	; Save code address for break
	LDB	RH0,@R3		; Get next byte in code string
	CPB	RH0,#':'	; Multi statement line?
	JP	Z,EXCUTE	; Yes - Execute it
	ORB	RH0,RH0
	RESFLG	C		; End of line?
	JP	NZ,SNERR	; No - Syntax error
	LDCTLB	RL4,FLAGS
	INC	R3		; Point to address of next line
	LDCTLB	FLAGS,RL4
	LDB	RH0,@R3		; Get LSB of line pointer
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	ORB	RH0,@R3
	RESFLG	C		; Is it zero (End of prog)?
	JP	Z,ENDPRG	; Yes - Terminate execution
	LDCTLB	RL4,FLAGS
	INC	R3		; Point to line number
	LDCTLB	FLAGS,RL4
	LDB	RL2,@R3		; Get LSB of line number
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	RH2,@R3		; Get MSB of line number
	EX	R3,R2		; Line number to HL
	LD	(LINEAT),R3	; Save as current line number
	EX	R3,R2		; Line number back to DE
EXCUTE:
	CALL	GETCHR		; Get key word
	LD	R2,#RUNCNT	; Where to RETurn to
	PUSH	@R15,R2		; Save for RETurn
IFJMP:
	RET	Z		; Go to RUNCNT if end of STMT
ONJMP:
	SUBB	RH0,#ZEND	; Is it a token?
	JP	C,LET		; No - try to assign it
	CPB	RH0,#ZNEW+1-ZEND; END to NEW ?
	JP	NC,SNERR	; Not a key word - ?SN Error
	RLB	RH0,#1		; Double it
	LDB	RL1,RH0		; BC = Offset into table
	LDB	RH1,#0
	EX	R3,R2		; Save code string address
	LD	R3,#WORDTB	; Keyword address table
	ADD	R3,R1		; Point to routine address
;	LDB	RL1,@R3		; Get LSB of routine address
	LDB	RH1,@R3		; Get LSB of routine address
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
;	LDB	RH1,@R3		; Get MSB of routine address
	LDB	RL1,@R3		; Get MSB of routine address
	PUSH	@R15,R1		; Save routine address
	EX	R3,R2		; Restore code string address
;
GETCHR:
	INC	R3		; Point to next character
	LDB	RH0,@R3		; Get next code string byte
	CPB	RH0,#':'	; Z if ':'
	RET	NC		; NC if > "9"
	CPB	RH0,#' '
	JP	Z,GETCHR	; Skip over spaces
	CPB	RH0,#'0'
	COMFLG	C		; NC if < '0'
	INCB	RH0,#1		; Test for zero - Leave carry
	DECB	RH0,#1		; Z if Null
	RET
;
RESTOR:
	EX	R3,R2		; Save code string address
	LD	R3,(BASTXT)	; Point to start of program
	JP	Z,RESTNL	; Just RESTORE - reset pointer
	EX	R3,R2		; Restore code string address
	CALL	ATOH		; Get line number to DE
	PUSH	@R15,R3		; Save code string address
	CALL	SRCHLN		; Search for line number in DE
	LDB	RH3,RH1		; HL = Address of line
	LDB	RL3,RL1
	POP	R2,@R15		; Restore code string address
	JP	NC,ULERR	; ?UL Error if not found
RESTNL:
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; Byte before DATA statement
	LDCTLB	FLAGS,RL4
UPDATA:
	LD	(NXTDAT),R3	; Update DATA pointer
	EX	R3,R2		; Restore code string address
	RET
;

TSTBRK:
	CALL	CONST		; Check input status
	RET	Z		; No key, go back
	CALL	CONIN		; Get the key into A
	CPB	RH0,#ESC	; Escape key?
	JR	Z,BRK		; Yes, break
	CPB	RH0,#CTRLC	; <Ctrl-C>
	JR	Z,BRK		; Yes, break
	CPB	RH0,#CTRLS	; Stop scrolling?
	RET	NZ		; Other key, ignore
;

STALL:
	CALL	CONIN		; Wait for key
	CPB	RH0,#CTRLQ	; Resume scrolling?
	RET	Z		; Release the chokehold
	CPB	RH0,#CTRLC	; Second break?
	JR	Z,STOP		; Break during hold exits prog
	JR	STALL		; Loop until <Ctrl-Q> or <brk>
;
BRK:
	LDB	RH0,#0FFH	; Set BRKFLG
	LDB	(BRKFLG),RH0	; Store it
;

STOP:
	RET	NZ		; Exit if anything else
	ORB	RH0,#11000000B	; Flag "STOP"
	RESFLG	C
	JR	PEND1
PEND:
	RET	NZ		; Exit if anything else
PEND1:
	LD	(BRKLIN),R3	; Save point of break
	JR	INPBRK1		; Skip "OR 11111111B"
INPBRK:
	ORB	RH0,#11111111B	; Flag "Break" wanted
	RESFLG	C
INPBRK1:
	POP	R1,@R15		; Return not needed and more
ENDPRG:
	LD	R3,(LINEAT)	; Get current line number
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Save STOP / END status
	LDB	RH0,RL3		; Is it direct break?
	ANDB	RH0,RH3
	RESFLG	C
	INCB	RH0,#1		; Line is -1 if direct break
	JP	Z,NOLIN		; Yes - No line number
	LD	(ERRLIN),R3	; Save line of break
	LD	R3,(BRKLIN)	; Get point of break
	LD	(CONTAD),R3	; Save point to CONTinue
NOLIN:
	XORB	RH0,RH0
	RESFLG	C
	LDB	(CTLOFG),RH0	; Enable output
	CALL	STTLIN		; Start a new line
	POP	R0,@R15		; Restore STOP / END status
	LDCTLB	FLAGS,RL0
	LD	R3,#BRKMSG	; "Break" message
	JP	NZ,ERRIN	; "in line" wanted?
	JP	PRNTOK		; Go to command mode
;
CONT:
	LD	R3,(CONTAD)	; Get CONTinue address
	LDB	RH0,RH3		; Is it zero?
	ORB	RH0,RL3
	RESFLG	C
	LDB	RL2,#CN		; ?CN Error
	JP	Z,ERROR		; Yes - output "?CN Error"
	EX	R3,R2		; Save code string address
	LD	R3,(ERRLIN)	; Get line of last break
	LD	(LINEAT),R3	; Set up current line number
	EX	R3,R2		; Restore code string address
	RET			; CONTinue where left off
;
NULL:
	CALL	GETINT		; Get integer 0-255
	RET	NZ		; Return if bad value
	LDB	(NULLS),RH0	; Set nulls number
	RET
;
ACCSUM:
	PUSH	@R15,R3		; Save address in array
	LD	R3,(CHKSUM)	; Get check sum
	LDB	RH1,#0		; BC - Value of byte
	LDB	RL1,RH0
	ADD	R3,R1		; Add byte to check sum
	LD	(CHKSUM),R3	; Re-save check sum
	POP	R3,@R15		; Restore address in array
	RET
;
CHKLTR:
	LDB	RH0,@R3		; Get byte
	CPB	RH0,#'A'	; < 'a' ?
	RET	C		; Carry set if not letter
	CPB	RH0,#'Z'+1	; > 'z' ?
	COMFLG	C
	RET			; Carry set if not letter
;
FPSINT:
	CALL	GETCHR		; Get next character
POSINT:
	CALL	GETNUM		; Get integer 0 to 32767
DEPINT:
	CALL	TSTSGN		; Test sign of FPREG
	JP	MI,FCERR	; Negative - ?FC Error
DEINT:
	LDB	RH0,(FPEXP)	; Get integer value to DE
	CPB	RH0,#80H+16	; Exponent in range (16 bits)?
	JP	C,FPINT		; Yes - convert it
	LD	R1,#9080H	; BCDE = -32768
	LD	R2,#0000H
	PUSH	@R15,R3		; Save code string address
	CALL	CMPNUM		; Compare FPREG with BCDE
	POP	R3,@R15		; Restore code string address
	LDB	RH2,RL1		; MSB to D
	RET	Z		; Return if in range
FCERR:
	LDB	RL2,#FC		; ?FC Error
	JP	ERROR		; Output error-
;
ATOH:
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; ASCII number to DE binary
	LDCTLB	FLAGS,RL4
GETLN:
	LD	R2,#0		; Get number to DE
GTLNLP:
	CALL	GETCHR		; Get next character
	RET	NC		; Exit if not a digit
	PUSH	@R15,R3		; Save code string address
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Save digit
	LD	R3,#65529/10	; Largest number 65529
	CALL	CPDEHL		; Number in range?
	JP	C,SNERR		; No - ?SN Error
	LDB	RH3,RH2		; HL = Number
	LDB	RL3,RL2
	ADD	R3,R2		; Times 2
	ADD	R3,R3		; Times 4
	ADD	R3,R2		; Times 5
	ADD	R3,R3		; Times 10
	POP	R0,@R15
	LDCTLB	FLAGS,RL0	; Restore digit
	SUBB	RH0,#'0'	; Make it 0 to 9
	LDB	RL2,RH0		; DE = Value of digit
	LDB	RH2,#0
	ADD	R3,R2		; Add to number
	EX	R3,R2		; Number to DE
	POP	R3,@R15		; Restore code string address
	JP	GTLNLP		; Go to next character
;
CLEAR:
	JP	Z,INTVAR	; Just "CLEAR" Keep parameters
	CALL	POSINT		; Get integer 0 to 32767 to DE
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; Cancel increment
	LDCTLB	FLAGS,RL4
	CALL	GETCHR		; Get next character
	PUSH	@R15,R3		; Save code string address
	LD	R3,(LSTRAM)	; Get end of RAM
	JP	Z,STORED	; No value given - Use stored
	POP	R3,@R15		; Restore code string address
	LD	RH4,#','
	CALL	CHKSYN		; Check for comma
	PUSH	@R15,R2		; Save number
	CALL	POSINT		; Get integer 0 to 32767
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; Cancel increment
	LDCTLB	FLAGS,RL4
	CALL	GETCHR		; Get next character
	JP	NZ,SNERR	; ?SN Error if more on line
	EX	@R15,R3		; Save code string address
	EX	R3,R2		; Number to DE
STORED:
	LDB	RH0,RL3		; Get LSB of new RAM top
	SUBB	RH0,RL2		; Subtract LSB of string space
	LDB	RL2,RH0		; Save LSB
	LDB	RH0,RH3		; Get MSB of new RAM top
	SBCB	RH0,RH2		; Subtract MSB of string space
	LDB	RH2,RH0		; Save MSB
	JP	C,OMERR		; ?OM Error if not enough mem
	PUSH	@R15,R3		; Save RAM top
	LD	R3,(PROGND)	; Get program end
	LD	R1,#40		; 40 Bytes minimum working RAM
	ADD	R3,R1		; Get lowest address
	CALL	CPDEHL		; Enough memory?
	JP	NC,OMERR	; No - ?OM Error
	EX	R3,R2		; RAM top to HL
	LD	(STRSPC),R3	; Set new string space
	POP	R3,@R15		; End of memory to use
	LD	(LSTRAM),R3	; Set new top of RAM
	POP	R3,@R15		; Restore code string address
	JP	INTVAR		; Initialise variables
;
RUN:
	JP	Z,RUNFST	; RUN from start if just RUN
	CALL	INTVAR		; Initialise variables
	LD	R1,#RUNCNT	; Execution driver loop
	JP	RUNLIN		; RUN from line number
;
GOSUB:
;	LDB	RL1,#3		; 3 Levels of stack needed
	LDB	RL1,#4		; 4 Levels of stack needed
	CALL	CHKSTK		; Check for 3 levels of stack
	POP	R1,@R15		; Get return address
	PUSH	@R15,R3		; Save code string for RETURN
	PUSH	@R15,R3		; And for GOSUB routine
	LD	R3,(LINEAT)	; Get current line
	EX	@R15,R3		; Into stack - Code string out
	LDB	RH0,#ZGOSUB	; "GOSUB" token
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Save token
;	INC	R15		; Don't save flags
;
RUNLIN:
	PUSH	@R15,R1		; Save return address
GOTO:
	CALL	ATOH		; ASCII number to DE binary
	CALL	REM		; Get end of line
	PUSH	@R15,R3		; Save end of line
	LD	R3,(LINEAT)	; Get current line
	CALL	CPDEHL		; Line after current?
	POP	R3,@R15		; Restore end of line
	LDCTLB	RL4,FLAGS
	INC	R3		; Start of next line
	LDCTLB	FLAGS,RL4
	JR	NC,GOTO1
	CALL	SRCHLP		; Line is after current line
GOTO1:
	JR	C,GOTO2
	CALL	SRCHLN		; Line is before current line
GOTO2:
	LDB	RH3,RH1		; Set up code string address
	LDB	RL3,RL1
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; Incremented after
	LDCTLB	FLAGS,RL4
	RET	C		; Line found
ULERR:
	LDB	RL2,#UL		; ?UL Error
	JP	ERROR		; Output error message
;
RETURN:
	RET	NZ		; Return if not just RETURN
	LDB	RH2,#-1		; Flag "GOSUB" search
	CALL	BAKSTK		; Look "GOSUB" block
	LD	R15,R3		; Kill all FORs in subroutine
	CPB	RH0,#ZGOSUB	; Test for "GOSUB" token
	LDB	RL2,#RG		; ?RG Error
	JP	NZ,ERROR	; Error if no "GOSUB" found
	POP	R3,@R15		; Get RETURN line number
	LD	(LINEAT),R3	; Save as current
	LDCTLB	RL4,FLAGS
	INC	R3		; Was it from direct statement?
	LDCTLB	FLAGS,RL4
	LDB	RH0,RH3
	ORB	RH0,RL3
	RESFLG	C		; Return to line
	JP	NZ,RETLIN	; No - Return to line
	LDB	RH0,(LSTBIN)	; Any INPUT in subroutine?
	ORB	RH0,RH0
	RESFLG	C		; If so buffer is corrupted
	JP	NZ,POPNOK	; Yes - Go to command mode
RETLIN:
	LD	R3,#RUNCNT	; Execution driver loop
	EX	@R15,R3		; Into stack - Code string out
	JR	DATA		; Skip "POP HL"
NXTDTA:
	POP	R3,@R15		; Restore code string address
;
DATA:
	LDB	RL1,#':'	; ':' End of statement
	JR	REM1
REM:
	LDB	RL1,#0		; 00	End of statement
REM1:
	LDB	RH1,#0
NXTSTL:
	LDB	RH0,RL1		; Statement and byte
	LDB	RL1,RH1
	LDB	RH1,RH0		; Statement end byte
NXTSTT:
	LDB	RH0,@R3		; Get byte
	ORB	RH0,RH0
	RESFLG	C		; End of line?
	RET	Z		; Yes - Exit
	CPB	RH0,RH1		; End of statement?
	RET	Z		; Yes - Exit
	LDCTLB	RL4,FLAGS
	INC	R3		; Next byte
	LDCTLB	FLAGS,RL4
	CPB	RH0,#'"'	; Literal string?
	JP	Z,NXTSTL	; Yes - Look for another '"'
	JP	NXTSTT		; Keep looking
;
LET:
	CALL	GETVAR		; Get variable name
	LD	RH4,#ZEQUAL	; "=" token
	CALL	CHKSYN		; Make sure "=" follows
	PUSH	@R15,R2		; Save address of variable
	LDB	RH0,(TYPE)	; Get data type
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Save type
	CALL	EVAL		; Evaluate expression
	POP	R0,@R15		; Restore type
	LDCTLB	FLAGS,RL0
	EX	@R15,R3		; Save code - Get var addr
	LD	(BRKLIN),R3	; Save address of variable
	RRCB	RH0,#1		; Adjust type
	CALL	CHKTYP		; Check types are the same
	JP	Z,LETNUM	; Numeric - Move value
LETSTR:
	PUSH	@R15,R3		; Save address of string var
	LD	R3,(FPREG)	; Pointer to string entry
	EXB	RH3,RL3
	PUSH	@R15,R3		; Save it on stack
	LDCTLB	RL4,FLAGS
	INC	R3		; Skip over length
	INC	R3
	LDB	RL2,@R3		; LSB of string address
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	RH2,@R3		; MSB of string address
	LD	R3,(BASTXT)	; Point to start of program
	CALL	CPDEHL		; Is string before program?
	JP	NC,CRESTR	; Yes - Create string entry
	LD	R3,(STRSPC)	; Point to string space
	CALL	CPDEHL		; Is string literal in program?
	POP	R2,@R15		; Restore address of string
	JP	NC,MVSTPT	; Yes - Set up pointer
	LD	R3,#TMPSTR	; Temporary string pool
	CALL	CPDEHL		; Is string in temporary pool?
	JP	NC,MVSTPT	; No - Set up pointer
	JR	CRESTR1		; Skip "POP DE"
CRESTR:
	POP	R2,@R15		; Restore address of string
CRESTR1:
	CALL	BAKTMP		; Back to last tmp-str entry
	EX	R3,R2		; Address of string entry
	CALL	SAVSTR		; Save string in string area
MVSTPT:
	CALL	BAKTMP		; Back to last tmp-str entry
	POP	R3,@R15		; Get string pointer
	CALL	DETHL4		; Move string pointer to var
	POP	R3,@R15		; Restore code string address
	RET
;
LETNUM:
	PUSH	@R15,R3		; Save address of variable
	CALL	FPTHL		; Move value to variable
	POP	R2,@R15		; Restore address of variable
	POP	R3,@R15		; Restore code string address
	RET
;
ON:
	CALL	GETINT		; Get integer 0-255
	LDB	RH0,@R3		; Get "GOTO" or "GOSUB" token
	LDB	RH1,RH0		; Save in B
	CPB	RH0,#ZGOSUB	; "GOSUB" token?
	JP	Z,ONGO		; Yes - Find line number
	LD	RH4,#ZGOTO	; "GOTO" token
	CALL	CHKSYN		; Make sure it's "GOTO"
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; Cancel increment
	LDCTLB	FLAGS,RL4
ONGO:
	LDB	RL1,RL2		; Integer of branch value
ONGOLP:
	DECB	RL1,#1		; Count branches
	LDB	RH0,RH1		; Get "GOTO" or "GOSUB" token
	JP	Z,ONJMP		; Go to that line if right one
	CALL	GETLN		; Get line number to DE
	CPB	RH0,#','	; Another line number?
	RET	NZ		; No - Drop through
	JP	ONGOLP		; Yes - loop
;
IF:
	CALL	EVAL		; Evaluate expression
	LDB	RH0,@R3		; Get token
	CPB	RH0,#ZGOTO	; "GOTO" token?
	JP	Z,IFGO		; Yes - Get line
	LD	RH4,#ZTHEN	; "THEN" token
	CALL	CHKSYN		; Make sure it's "THEN"
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; Cancel increment
	LDCTLB	FLAGS,RL4
IFGO:
	CALL	TSTNUM		; Make sure it's numeric
	CALL	TSTSGN		; Test state of expression
	JP	Z,REM		; False - Drop through
	CALL	GETCHR		; Get next character
	JP	C,GOTO		; Number - GOTO that line
	JP	IFJMP		; Otherwise do statement
;
MRPRNT:
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; DEC 'cos GETCHR INCs
	LDCTLB	FLAGS,RL4
	CALL	GETCHR		; Get next character
PRINT:
	JP	Z,PRCRLF	; CRLF if just PRINT
PRNTLP:
	RET	Z		; End of list - Exit
	CPB	RH0,#ZTAB	; "TAB(" token?
	JP	Z,DOTAB		; Yes - Do TAB routine
	CPB	RH0,#ZSPC	; "SPC(" token?
	JP	Z,DOTAB		; Yes - Do SPC routine
	PUSH	@R15,R3		; Save code string address
	CPB	RH0,#','	; Comma?
	JP	Z,DOCOM		; Yes - Move to next zone
	CPB	RH0,#";"	; Semi-colon?
	JP	Z,NEXITM	; Do semi-colon routine
	POP	R1,@R15		; Code string address to BC
	CALL	EVAL		; Evaluate expression
	PUSH	@R15,R3		; Save code string address
	LDB	RH0,(TYPE)	; Get variable type
	ORB	RH0,RH0
	RESFLG	C		; Is it a string variable?
	JP	NZ,PRNTST	; Yes - Output string contents
	CALL	NUMASC		; Convert number to text
	CALL	CRTST		; Create temporary string
	LDB	@R3,#' '	; Followed by a space
	LD	R3,(FPREG)	; Get length of output
	EXB	RH3,RL3
	INCB	@R3,#1		; Plus 1 for the space
	LD	R3,(FPREG)	; < Not needed >
	EXB	RH3,RL3
	LDB	RH0,(LWIDTH)	; Get width of line
	LDB	RH1,RH0		; To B
	INCB	RH1,#1		; Width 255 (No limit)?
	JP	Z,PRNTNB	; Yes - Output number string
	INCB	RH1,#1		; Adjust it
	LDB	RH0,(CURPOS)	; Get cursor position
	ADDB	RH0,@R3		; Add length of string
	DECB	RH0,#1		; Adjust it
	CPB	RH0,RH1		; Will output fit on this line?
	JR	C,PRNTNB
	CALL	PRCRLF		; No - CRLF first
PRNTNB:
	CALL	PRS1		; Output string at (HL)
	XORB	RH0,RH0
	RESFLG	C		; Skip CALL by setting 'z' flag
PRNTST:
	JR	Z,PRNTST1
	CALL	PRS1		; Output string at (HL)
PRNTST1:
	POP	R3,@R15		; Restore code string address
	JP	MRPRNT		; See if more to PRINT
;
STTLIN:
	LDB	RH0,(CURPOS)	; Make sure on new line
	ORB	RH0,RH0
	RESFLG	C		; Already at start?
	RET	Z		; Yes - Do nothing
	JP	PRCRLF		; Start a new line
;
ENDINP:
	LDB	@R3,#0		; Mark end of buffer
	LD	R3,#BUFFER-1	; Point to buffer
PRCRLF:
	LDB	RH0,#CR		; Load a CR
	CALL	OUTC		; Output character
	LDB	RH0,#LF		; Load a LF
	CALL	OUTC		; Output character
DONULL:
	XORB	RH0,RH0
	RESFLG	C		; Set to position 0
	LDB	(CURPOS),RH0	; Store it
	LDB	RH0,(NULLS)	; Get number of nulls
NULLP:
	DECB	RH0,#1		; Count them
	RET	Z		; Return if done
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Save count
	XORB	RH0,RH0
	RESFLG	C		; Load a null
	CALL	OUTC		; Output it
	POP	R0,@R15		; Restore count
	LDCTLB	FLAGS,RL0
	JP	NULLP		; Keep counting
;
DOCOM:
	LDB	RH0,(COMMAN)	; Get comma width
	LDB	RH1,RH0		; Save in B
	LDB	RH0,(CURPOS)	; Get current position
	CPB	RH0,RH1		; Within the limit?
	JR	C,DOCOM1
	CALL	PRCRLF		; No - output CRLF
DOCOM1:
	JP	NC,NEXITM	; Get next item
ZONELP:
	SUBB	RH0,#14		; Next zone of 14 characters
	JP	NC,ZONELP	; Repeat if more zones
	LDCTLB	RL4,FLAGS
	COMB	RH0		; Number of spaces to output
	LDCTLB	FLAGS,RL4
	JP	ASPCS		; Output them
;
DOTAB:
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Save token
	CALL	FNDNUM		; Evaluate expression
	LD	RH4,#")"
	CALL	CHKSYN		; Make sure ")" follows
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; Back space on to ")"
	LDCTLB	FLAGS,RL4
	POP	R0,@R15		; Restore token
	LDCTLB	FLAGS,RL0
	SUBB	RH0,#ZSPC	; Was it "SPC(" ?
	PUSH	@R15,R3		; Save code string address
	JP	Z,DOSPC		; Yes - Do 'E' spaces
	LDB	RH0,(CURPOS)	; Get current position
DOSPC:
	LDCTLB	RL4,FLAGS
	COMB	RH0		; Number of spaces to print to
	LDCTLB	FLAGS,RL4
	ADDB	RH0,RL2		; Total number to print
	JP	NC,NEXITM	; TAB < Current POS(X)
ASPCS:
	INCB	RH0,#1		; Output A spaces
	LDB	RH1,RH0		; Save number to print
	LDB	RH0,#' '	; Space
SPCLP:
	CALL	OUTC		; Output character in A
	DECB	RH1,#1		; Count them
	JP	NZ,SPCLP	; Repeat if more
NEXITM:
	POP	R3,@R15		; Restore code string address
	CALL	GETCHR		; Get next character
	JP	PRNTLP		; More to print
;
REDO:
	DB	"?Redo from start",CR,LF,0
;
	ALIGN	2
BADINP:
	LDB	RH0,(READFG)	; READ or INPUT?
	ORB	RH0,RH0
	RESFLG	C
	JP	NZ,DATSNR	; READ - ?SN Error
	POP	R1,@R15		; Throw away code string addr
	LD	R3,#REDO	; "Redo from start" message
	CALL	PRS		; Output string
	JP	DOAGN		; Do last INPUT again
;
INPUT:
	CALL	IDTEST		; Test for illegal direct
	LDB	RH0,@R3		; Get character after "INPUT"
	CPB	RH0,#'"'	; Is there a prompt string?
	LDB	RH0,#0		; Clear A and leave flags
	LDB	(CTLOFG),RH0	; Enable output
	JP	NZ,NOPMPT	; No prompt - get input
	CALL	QTSTR		; Get string terminated by '"'
	LD	RH4,#';'
	CALL	CHKSYN		; Check for ';' after prompt
	PUSH	@R15,R3		; Save code string address
	CALL	PRS1		; Output prompt string
	JR	NOPMPT1		; Skip "PUSH HL"
NOPMPT:
	PUSH	@R15,R3		; Save code string address
NOPMPT1:
	CALL	PROMPT		; Get input with "? " prompt
	POP	R1,@R15		; Restore code string address
	JP	C,INPBRK	; Break pressed - Exit
	LDCTLB	RL4,FLAGS
	INC	R3		; Next byte
	LDCTLB	FLAGS,RL4
	LDB	RH0,@R3		; Get it
	ORB	RH0,RH0
	RESFLG	C		; End of line?
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; Back again
	LDCTLB	FLAGS,RL4
	PUSH	@R15,R1		; Re-save code string address
	JP	Z,NXTDTA	; Yes - Find next DATA stmt
	LDB	@R3,#','	; Store comma as separator
	JP	NXTITM		; Get next item
;
READ:
	PUSH	@R15,R3		; Save code string address
	LD	R3,(NXTDAT)	; Next DATA statement
	ORB	RH0,#0AFH
	RESFLG	C
	JR	NXTITM1		; Flag "READ"
NXTITM:
	XORB	RH0,RH0
	RESFLG	C		; Flag "INPUT"
NXTITM1:
	LDB	(READFG),RH0	; Save "READ"/"INPUT" flag
	EX	@R15,R3		; Get code str' , Save pointer
	JP	GTVLUS		; Get values
;
NEDMOR:
	LD	RH4,#','
	CALL	CHKSYN		; Check for comma between items
GTVLUS:
	CALL	GETVAR		; Get variable name
	EX	@R15,R3		; Save code str" , Get pointer
	PUSH	@R15,R2		; Save variable address
	LDB	RH0,@R3		; Get next "INPUT"/"DATA" byte
	CPB	RH0,#','	; Comma?
	JP	Z,ANTVLU	; Yes - Get another value
	LDB	RH0,(READFG)	; Is it READ?
	ORB	RH0,RH0
	RESFLG	C
	JP	NZ,FDTLP	; Yes - Find next DATA stmt
	LDB	RH0,#'?'	; More INPUT needed
	CALL	OUTC		; Output character
	CALL	PROMPT		; Get INPUT with prompt
	POP	R2,@R15		; Variable address
	POP	R1,@R15		; Code string address
	JP	C,INPBRK	; Break pressed
	LDCTLB	RL4,FLAGS
	INC	R3		; Point to next DATA byte
	LDCTLB	FLAGS,RL4
	LDB	RH0,@R3		; Get byte
	ORB	RH0,RH0
	RESFLG	C		; Is it zero (No input) ?
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; Back space INPUT pointer
	LDCTLB	FLAGS,RL4
	PUSH	@R15,R1		; Save code string address
	JP	Z,NXTDTA	; Find end of buffer
	PUSH	@R15,R2		; Save variable address
ANTVLU:
	LDB	RH0,(TYPE)	; Check data type
	ORB	RH0,RH0
	RESFLG	C		; Is it numeric?
	JP	Z,INPBIN	; Yes - Convert to binary
	CALL	GETCHR		; Get next character
	LDB	RH2,RH0		; Save input character
	LDB	RH1,RH0		; Again
	CPB	RH0,#'"'	; Start of literal sting?
	JP	Z,STRENT	; Yes - Create string entry
	LDB	RH0,(READFG)	; "READ" or "INPUT" ?
	ORB	RH0,RH0
	RESFLG	C
	LDB	RH2,RH0		; Save 00 if "INPUT"
	JP	Z,ITMSEP	; "INPUT" - End with 00
	LDB	RH2,#':'	; "DATA" - End with 00 or ':'
ITMSEP:
	LDB	RH1,#','	; Item separator
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; Back space for DTSTR
	LDCTLB	FLAGS,RL4
STRENT:
	CALL	DTSTR		; Get string terminated by D
	EX	R3,R2		; String address to DE
	LD	R3,#LTSTND	; Where to go after LETSTR
	EX	@R15,R3		; Save HL , get input pointer
	PUSH	@R15,R2		; Save address of string
	JP	LETSTR		; Assign string to variable
;
INPBIN:
	CALL	GETCHR		; Get next character
	CALL	ASCTFP		; Convert ASCII to FP number
	EX	@R15,R3		; Save input ptr, Get var addr
	CALL	FPTHL		; Move FPREG to variable
	POP	R3,@R15		; Restore input pointer
LTSTND:
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; DEC 'cos GETCHR INCs
	LDCTLB	FLAGS,RL4
	CALL	GETCHR		; Get next character
	JP	Z,MORDT		; End of line - More needed?
	CPB	RH0,#','		; Another value?
	JP	NZ,BADINP	; No - Bad input
MORDT:
	EX	@R15,R3		; Get code string address
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; DEC 'cos GETCHR INCs
	LDCTLB	FLAGS,RL4
	CALL	GETCHR		; Get next character
	JP	NZ,NEDMOR	; More needed - Get it
	POP	R2,@R15		; Restore DATA pointer
	LDB	RH0,(READFG)	; "READ" or "INPUT" ?
	ORB	RH0,RH0
	RESFLG	C
	EX	R3,R2		; DATA pointer to HL
	JP	NZ,UPDATA	; Update DATA pointer if "READ"
	PUSH	@R15,R2		; Save code string address
	ORB	RH0,@R3
	RESFLG	C		; More input given?
	LD	R3,#EXTIG	; "?Extra ignored" message
	JR	Z,MORDT1
	CALL	PRS		; Output string if extra given
MORDT1:
	POP	R3,@R15		; Restore code string address
	RET
;
EXTIG:
	DB	"?Extra ignored",CR,LF,0
;
	ALIGN	2
FDTLP:
	CALL	DATA		; Get next statement
	ORB	RH0,RH0
	RESFLG	C		; End of line?
	JP	NZ,FANDT	; No - See if DATA statement
	LDCTLB	RL4,FLAGS
	INC	R3
	LDB	RH0,@R3		; End of program?
	INC	R3
	LDCTLB	FLAGS,RL4
	ORB	RH0,@R3
	RESFLG	C		; 00 00 Ends program
	LDB	RL2,#OD		; ?OD Error
	JP	Z,ERROR		; Yes - Out of DATA
	LDCTLB	RL4,FLAGS
	INC	R3
	LDB	RL2,@R3		; LSB of line number
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	RH2,@R3		; MSB of line number
	EX	R3,R2
	LD	(DATLIN),R3	; Set line of current DATA item
	EX	R3,R2
FANDT:
	CALL	GETCHR		; Get next character
	CPB	RH0,#ZDATA	; "DATA" token
	JP	NZ,FDTLP	; No "DATA" - Keep looking
	JP	ANTVLU		; Found - Convert input
;
NEXT:
	LD	R2,#0		; In case no index given
NEXT1:
	JR	Z,NEXT2
	CALL	GETVAR		; Get index address
NEXT2:
	LD	(BRKLIN),R3	; Save code string address
	CALL	BAKSTK		; Look for "FOR" block
	JP	NZ,NFERR	; No "FOR" - ?NF Error
	LD	R15,R3		; Clear nested loops
	EXB	RH2,RL2
	PUSH	@R15,R2		; Save index address
	LDB	RH0,@R3		; Get sign of STEP
	INC	R3
	INC	R3
	PUSH	@R15,R0		; Save sign of STEP
	PUSH	@R15,R2		; Save index address
	CALL	PHLTFP		; Move index value to FPREG
	EX	@R15,R3		; Save address of TO value
	EXB	RH3,RL3
	PUSH	@R15,R3		; Save address of index
	CALL	ADDPHL		; Add STEP to index value
	POP	R3,@R15		; Restore address of index
	CALL	FPTHL		; Move value to index variable
	POP	R3,@R15		; Restore address of TO value
	CALL	LOADFP		; Move TO value to BCDE
	PUSH	@R15,R3		; Save address of line of FOR
	CALL	CMPNUM		; Compare index with TO value
	POP	R3,@R15		; Restore address of line num
	POP	R1,@R15		; Address of sign of STEP
	SUBB	RH0,RH1		; Compare with expected sign
	CALL	LOADFP		; BC = Loop stmt,DE = Line num
	EXB	RH1,RL1
	EXB	RH2,RL2
	JP	Z,KILFOR	; Loop finished - Terminate it
	EX	R3,R2		; Loop statement line number
	LD	(LINEAT),R3	; Set loop line number
	LDB	RL3,RL1		; Set code string to loop
	LDB	RH3,RH1
	JP	PUTFID		; Put back "FOR" and continue
;
KILFOR:
	LD	R15,R3		; Remove "FOR" block
	LD	R3,(BRKLIN)	; Code string after "NEXT"
	LDB	RH0,@R3		; Get next byte in code string
	CPB	RH0,#','	; More NEXTs ?
	JP	NZ,RUNCNT	; No - Do next statement
	CALL	GETCHR		; Position to index name
	CALL	NEXT1		; Re-enter NEXT routine
; < will not RETurn to here , Exit to RUNCNT or Loop >
;
GETNUM:
	CALL	EVAL		; Get a numeric expression
TSTNUM:
	RESFLG	C
	JR	CHKTYP		; Clear carry (numeric)
TSTSTR:
	SETFLG	C		; Set carry (string)
CHKTYP:
	LDB	RH0,(TYPE)	; Check types match
	ADCB	RH0,RH0		; Expected + actual
	ORB	RH0,RH0
	RESFLG	C		; Clear carry , set parity
	RET	PE		; Even parity - Types match
	JP	TMERR		; Different types - Error
;
OPNPAR:
	LD	RH4,#"("
	CALL	CHKSYN		; Make sure "(" follows
EVAL:
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; Evaluate expression & save
	LDCTLB	FLAGS,RL4
	LDB	RH2,#0		; Precedence value
EVAL1:
	PUSH	@R15,R2		; Save precedence
	LDB	RL1,#1
	CALL	CHKSTK		; Check for 1 level of stack
	CALL	OPRND		; Get next expression value
EVAL2:
	LD	(NXTOPR),R3	; Save address of next operator
EVAL3:
	LD	R3,(NXTOPR)	; Restore address of next opr
	POP	R1,@R15		; Precedence value and operator
	LDB	RH0,RH1		; Get precedence value
	CPB	RH0,#78H	; "AND" or "OR" ?
	JR	C,EVAL4
	CALL	TSTNUM		; No - Make sure it's a number
EVAL4:
	LDB	RH0,@R3		; Get next operator / function
	LDB	RH2,#0		; Clear Last relation
RLTLP:
	SUBB	RH0,#ZGTR	; ">" Token
	JP	C,FOPRND	; + - * / ^ AND OR - Test it
	CPB	RH0,#ZLTH+1-ZGTR; < = >
	JP	NC,FOPRND	; Function - Call it
	CPB	RH0,#ZEQUAL-ZGTR; "="
	RLCB	RH0,#1		; <- Test for legal
	XORB	RH0,RH2
	RESFLG	C		; <- combinations of < = >
	CPB	RH0,RH2		; <- by combining last token
	LDB	RH2,RH0		; <- with current one
	JP	C,SNERR		; Error if "<<' '==" or ">>"
	LD	(CUROPR),R3	; Save address of current token
	CALL	GETCHR		; Get next character
	JP	RLTLP		; Treat the two as one
;
FOPRND:
	LDB	RH0,RH2		; < = > found ?
	ORB	RH0,RH0
	RESFLG	C
	JP	NZ,TSTRED	; Yes - Test for reduction
	LDB	RH0,@R3		; Get operator token
	LD	(CUROPR),R3	; Save operator address
	SUBB	RH0,#ZPLUS	; Operator or function?
	RET	C		; Neither - Exit
	CPB	RH0,#ZOR+1-ZPLUS; Is it + - * / ^ AND OR ?
	RET	NC		; No - Exit
	LDB	RL2,RH0		; Coded operator
	LDB	RH0,(TYPE)	; Get data type
	DECB	RH0,#1		; FF = numeric , 00 = string
	ORB	RH0,RL2
	RESFLG	C		; Combine with coded operator
	LDB	RH0,RL2		; Get coded operator
	JP	Z,CONCAT	; String concatenation
	RLB	RH0,#1		; Times 2
;	ADDB	RH0,RL2		; Times 3
	RLB	RH0,#1		; Times 4 @@ word aligned
	LDB	RL2,RH0		; To DE (D is 0)
	LD	R3,#PRITAB	; Precedence table
	ADD	R3,R2		; To the operator concerned
	LDB	RH0,RH1		; Last operator precedence
	LDB	RH2,@R3		; Get evaluation precedence
	CPB	RH0,RH2		; Compare with eval precedence
	RET	NC		; Exit if higher precedence
	LDCTLB	RL4,FLAGS
	INC	R3		; Point to routine address
	LDCTLB	FLAGS,RL4
	CALL	TSTNUM		; Make sure it's a number
;
STKTHS:
	PUSH	@R15,R1		; Save last precedence & token
	LD	R1,#EVAL3	; Where to go on prec' break
	PUSH	@R15,R1		; Save on stack for return
	LDB	RH1,RL2		; Save operator
	LDB	RL1,RH2		; Save precedence
	CALL	STAKFP		; Move value to stack
	LDB	RL2,RH1		; Restore operator
	LDB	RH2,RL1		; Restore precedence
	INC	R3		; @@ word aligned
	LDCTLB	FLAGS,RL4
;	LDB	RL1,@R3		; Get LSB of routine address
	LDB	RH1,@R3		; @@@@ Get LSB of routine address
	INC	R3
;	LDB	RH1,@R3		; Get MSB of routine address
	LDB	RL1,@R3		; @@@@ Get MSB of routine address
	INC	R3
	LDCTLB	FLAGS,RL4
	PUSH	@R15,R1		; Save routine address
	LD	R3,(CUROPR)	; Address of current operator
	JP	EVAL1		; Loop until prec' break
;
OPRND:
	XORB	RH0,RH0
	RESFLG	C		; Get operand routine
	LDB	(TYPE),RH0	; Set numeric expected
	CALL	GETCHR		; Get next character
	LDB	RL2,#MO		; ?MO Error
	JP	Z,ERROR		; No operand - Error
	JP	C,ASCTFP	; Number - Get value
	CALL	CHKLTR		; See if a letter
	JP	NC,CONVAR	; Letter - Find variable
	CPB	RH0,#'&'	; &H = HEX, &B = BINARY
	JR	NZ,NOTAMP
	CALL	GETCHR		; Get next character
	CPB	RH0,#'H'	; Hex number indicated? [function added]
	JP	Z,HEXTFP	; Convert Hex to FPREG
	CPB	RH0,#'B'	; Binary number indicated? [function added]
	JP	Z,BINTFP	; Convert Bin to FPREG
	LDB	RL2,#SN		; If neither then a ?SN Error
	JP	Z,ERROR
NOTAMP:
	CPB	RH0,#ZPLUS	; '+' Token ?
	JP	Z,OPRND		; Yes - Look for operand
	CPB	RH0,#'.'	; '.' ?
	JP	Z,ASCTFP	; Yes - Create FP number
	CPB	RH0,#ZMINUS	; '-' Token ?
	JP	Z,MINUS		; Yes - Do minus
	CPB	RH0,#'"'	; Literal string ?
	JP	Z,QTSTR		; Get string terminated by '"'
	CPB	RH0,#ZNOT	; "NOT" Token ?
	JP	Z,EVNOT		; Yes - Eval NOT expression
	CPB	RH0,#ZFN	; "FN" Token ?
	JP	Z,DOFN		; Yes - Do FN routine
	SUBB	RH0,#ZSGN	; Is it a function?
	JP	NC,FNOFST	; Yes - Evaluate function
EVLPAR:
	CALL	OPNPAR		; Evaluate expression in "()"
	LD	RH4,#")"
	CALL	CHKSYN		; Make sure ")" follows
	RET
;
MINUS:
	LDB	RH2,#7DH	; '-' precedence
	CALL	EVAL1		; Evaluate until prec' break
	LD	R3,(NXTOPR)	; Get next operator address
	PUSH	@R15,R3		; Save next operator address
	CALL	INVSGN		; Negate value
RETNUM:
	CALL	TSTNUM		; Make sure it's a number
	POP	R3,@R15		; Restore next operator address
	RET
;
CONVAR:
	CALL	GETVAR		; Get variable address to DE
FRMEVL:
	PUSH	@R15,R3		; Save code string address
	EX	R3,R2		; Variable address to HL
	EXB	RH3,RL3
	LD	(FPREG),R3	; Save address of variable
	EXB	RH3,RL3
	LDB	RH0,(TYPE)	; Get type
	ORB	RH0,RH0
	RESFLG	C		; Numeric?
	JR	NZ,FRMEVL1
	CALL	PHLTFP		; Yes - Move contents to FPREG
FRMEVL1:
	POP	R3,@R15		; Restore code string address
	RET
;
FNOFST:
	LDB	RH1,#0		; Get address of function
	RLB	RH0,#1		; Double function offset
	LDB	RL1,RH0		; BC = Offset in function table
	PUSH	@R15,R1		; Save adjusted token value
	CALL	GETCHR		; Get next character
	LDB	RH0,RL1		; Get adjusted token value
	CPB	RH0,#2*(ZLEFT-ZSGN)-1; Adj' LEFT$,RIGHT$ or MID$ ?
	JP	C,FNVAL		; No - Do function
	CALL	OPNPAR		; Evaluate expression	(X,...
	LD	RH4,#','
	CALL	CHKSYN		; Make sure ',' follows
	CALL	TSTSTR		; Make sure it's a string
	EX	R3,R2		; Save code string address
	LD	R3,(FPREG)	; Get address of string
	EXB	RH3,RL3
	EX	@R15,R3		; Save address of string
	PUSH	@R15,R3		; Save adjusted token value
	EX	R3,R2		; Restore code string address
	CALL	GETINT		; Get integer 0-255
	EX	R3,R2		; Save code string address
	EX	@R15,R3		; Save integer,HL = adj' token
	JP	GOFUNC		; Jump to string function
;
FNVAL:
	CALL	EVLPAR		; Evaluate expression
	EX	@R15,R3		; HL = Adjusted token value
	LD	R2,#RETNUM	; Return number from function
	PUSH	@R15,R2		; Save on stack
GOFUNC:
	LD	R1,#FNCTAB	; Function routine addresses
	ADD	R3,R1		; Point to right address
;	LDB	RL1,@R3		; Get LSB of address
	LDB	RH1,@R3		; Get LSB of address
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4	;
;	LDB	RH3,@R3		; Get MSB of address
	LDB	RL3,@R3		; Get MSB of address
	LDB	RH3,RH1		; Address to HL
	JP	@R3		; Jump to function
;
SGNEXP:
	DECB	RH2,#1		; Dee to flag negative exponent
	CPB	RH0,#ZMINUS	; '-' token ?
	RET	Z		; Yes - Return
	CPB	RH0,#'-'	; '-' ASCII ?
	RET	Z		; Yes - Return
	INCB	RH2,#1		; Inc to flag positive exponent
	CPB	RH0,#'+'	; '+' ASCII ?
	RET	Z		; Yes - Return
	CPB	RH0,#ZPLUS	; '+' token ?
	RET	Z		; Yes - Return
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; DEC 'cos GETCHR INCs
	LDCTLB	FLAGS,RL4
	RET			; Return "NZ"
;
POR:
	ORB	RH0,#0AFH	; Flag "OR"
	RESFLG	C
	JR	PAND1
PAND:
	XORB	RH0,RH0
	RESFLG	C		; Flag "AND"
PAND1:
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Save "AND" / "OR" flag
	CALL	TSTNUM		; Make sure it's a number
	CALL	DEINT		; Get integer -32768 to 32767
	POP	R0,@R15		; Restore "AND" / "OR" flag
	LDCTLB	FLAGS,RL0
	EX	R3,R2		; <- Get last
	POP	R1,@R15		; <- value
	EX	@R15,R3		; <- from
	EX	R3,R2		; <- stack
	CALL	FPBCDE		; Move last value to FPREG
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Save "AND" / "OR" flag
	CALL	DEINT		; Get integer -32768 to 32767
	POP	R0,@R15		; Restore "AND" / "OR" flag
	LDCTLB	FLAGS,RL0
	POP	R1,@R15		; Get value
	LDB	RH0,RL1		; Get LSB
	LD	R3,#ACPASS	; Address of save AC as current
	JP	NZ,POR1		; Jump if OR
	ANDB	RH0,RL2
	RESFLG	C		; "AND" LSBs
	LDB	RL1,RH0		; Save LSB
	LDB	RH0,RH1		; Get MBS
	ANDB	RH0,RH2
	RESFLG	C		; "AND" MSBs
	JP	@R3		; Save AC as current (ACPASS)
;
POR1:
	ORB	RH0,RL2
	RESFLG	C		; "OR" LSBs
	LDB	RL1,RH0		; Save LSB
	LDB	RH0,RH1		; Get MSB
	ORB	RH0,RH2
	RESFLG	C		; "OR" MSBs
	JP	@R3		; Save AC as current (ACPASS)
;
TSTRED:
	LD	R3,#CMPLOG	; Logical compare routine
	LDB	RH0,(TYPE)	; Get data type
	RRCB	RH0,#1		; Carry set = string
	LDB	RH0,RH2		; Get last precedence value
	RLCB	RH0,#1		; Times 2 plus carry
	LDB	RL2,RH0		; To E
	LDB	RH2,#64H	; Relational precedence
	LDB	RH0,RH1		; Get current precedence
	CPB	RH0,RH2		; Compare with last
	RET	NC		; Eval if last was rel' or log'
	JP	STKTHS		; Stack this one and get next
;
CMPLOG:
	DB	0
	DW	CMPLG1		; Compare two values / strings
	DB	0
CMPLG1:
	LDB	RH0,RL1		; Get data type
	ORB	RH0,RH0
	RESFLG	C
	RRCB	RH0,#1
	POP	R1,@R15		; Get last expression to BCDE
	POP	R2,@R15
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Save status
	CALL	CHKTYP		; Check that types match
	LD	R3,#CMPRES	; Result to comparison
	PUSH	@R15,R3		; Save for RETurn
	JP	Z,CMPNUM	; Compare values if numeric
	XORB	RH0,RH0
	RESFLG	C		; Compare two strings
	LDB	(TYPE),RH0	; Set type to numeric
	PUSH	@R15,R2		; Save string name
	CALL	GSTRCU		; Get current string
	LDB	RH0,@R3		; Get length of string
	LDCTLB	RL4,FLAGS
	INC	R3
	INC	R3
	LDB	RL1,@R3		; Get LSB of address
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	RH1,@R3		; Get MSB of address
	POP	R2,@R15		; Restore string name
	PUSH	@R15,R1		; Save address of string
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Save length of string
	CALL	GSTRDE		; Get second string
	CALL	LOADFP		; Get address of second string
	POP	R0,@R15		; Restore length of string 1
	LDCTLB	FLAGS,RL0
	LDB	RH2,RH0		; Length to D
	POP	R3,@R15		; Restore address of string 1
CMPSTR:
	LDB	RH0,RL2		; Bytes of string 2 to do
	ORB	RH0,RH2
	RESFLG	C		; Bytes of string 1 to do
	RET	Z		; Exit if all bytes compared
	LDB	RH0,RH2		; Get bytes of string 1 to do
	SUBB	RH0,#1
	RET	C		; Exit if end of string 1
	XORB	RH0,RH0
	RESFLG	C
	CPB	RH0,RL2		; Bytes of string 2 to do
	INCB	RH0,#1
	RET	NC		; Exit if end of string 2
	DECB	RH2,#1		; Count bytes in string 1
	DECB	RL2,#1		; Count bytes in string 2
	LDB	RH0,@R1		; Byte in string 2
	CPB	RH0,@R3		; Compare to byte in string 1
	LDCTLB	RL4,FLAGS
	INC	R3		; Move up string 1
	INC	R1		; Move up string 2
	LDCTLB	FLAGS,RL4
	JP	Z,CMPSTR	; Same - Try next bytes
	COMFLG	C		; Flag difference (">" or "<")
	JP	FLGDIF		; "<" gives -1 , ">" gives +1
;
CMPRES:
	INCB	RH0,#1		; Increment current value
	ADCB	RH0,RH0		; Double plus carry
	POP	R1,@R15		; Get other value
	ANDB	RH0,RH1
	RESFLG	C		; Combine them
	ADDB	RH0,#-1		; Carry set if different
	SBCB	RH0,RH0		; 00 - Equal , FF - Different
	JP	FLGREL		; Set current value & continue
;
EVNOT:
	LDB	RH2,#5AH	; Precedence value for "NOT"
	CALL	EVAL1		; Eval until precedence break
	CALL	TSTNUM		; Make sure it's a number
	CALL	DEINT		; Get integer -32768 - 32767
	LDB	RH0,RL2		; Get LSB
	LDCTLB	RL4,FLAGS
	COMB	RH0		; Invert LSB
	LDB	RL1,RH0		; Save "NOT" of LSB
	LDB	RH0,RH2		; Get MSB
	COMB	RH0		; Invert MSB
	LDCTLB	FLAGS,RL4
	CALL	ACPASS		; Save AC as current
	POP	R1,@R15		; Clean up stack
	JP	EVAL3		; Continue evaluation
;
DIMRET:
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; DEC 'cos GETCHR INCs
	LDCTLB	FLAGS,RL4
	CALL	GETCHR		; Get next character
	RET	Z		; End of DIM statement
	LD	RH4,#','
	CALL	CHKSYN		; Make sure ',' follows
DIM:
	LD	R1,#DIMRET	; Return to "DIMRET"
	PUSH	@R15,R1		; Save on stack
	ORB	RH0,#0AFH	; Flag "Create" variable
	RESFLG	C
	JR	GETVAR1
GETVAR:
	XORB	RH0,RH0
	RESFLG	C		; Find variable address,to DE
GETVAR1:
	LDB	(LCRFLG),RH0	; Set locate / create flag
	LDB	RH1,@R3		; Get First byte of name
GTFNAM:
	CALL	CHKLTR		; See if a letter
	JP	C,SNERR		; ?SN Error if not a letter
	XORB	RH0,RH0
	RESFLG	C
	LDB	RL1,RH0		; Clear second byte of name
	LDB	(TYPE),RH0	; Set type to numeric
	CALL	GETCHR		; Get next character
	JP	C,SVNAM2	; Numeric - Save in name
	CALL	CHKLTR		; See if a letter
	JP	C,CHARTY	; Not a letter - Check type
SVNAM2:
	LDB	RL1,RH0		; Save second byte of name
ENDNAM:
	CALL	GETCHR		; Get next character
	JP	C,ENDNAM	; Numeric - Get another
	CALL	CHKLTR		; See if a letter
	JP	NC,ENDNAM	; Letter - Get another
CHARTY:
	SUBB	RH0,#'$'	; String variable?
	JP	NZ,NOTSTR	; No - Numeric variable
	INCB	RH0,#1		; A = 1 (string type)
	LDB	(TYPE),RH0	; Set type to string
	RRB	RH0,#1		; A = 80H , Flag for string
	ADDB	RH0,RL1		; 2nd byte of name has bit 7 on
	LDB	RL1,RH0		; Resave second byte on name
	CALL	GETCHR		; Get next character
NOTSTR:
	LDB	RH0,(FORFLG)	; Array name needed ?
	DECB	RH0,#1
	JP	Z,ARLDSV	; Yes - Get array name
	JP	PL,NSCFOR	; No array with "FOR" or "FN"
	LDB	RH0,@R3		; Get byte again
	SUBB	RH0,#'('	; Subscripted variable?
	JP	Z,SBSCPT	; Yes - Sort out subscript
;
NSCFOR:
	XORB	RH0,RH0
	RESFLG	C		; Simple variable
	LDB	(FORFLG),RH0	; Clear "FOR" flag
	PUSH	@R15,R3		; Save code string address
	LDB	RH2,RH1		; DE = Variable name to find
	LDB	RL2,RL1
	LD	R3,(FNRGNM)	; FN argument name
	CALL	CPDEHL		; Is it the FN argument?
	LD	R2,#FNARG	; Point to argument value
	JP	Z,POPHRT	; Yes - Return FN argument value
	LD	R3,(VAREND)	; End of variables
	EX	R3,R2		; Address of end of search
	LD	R3,(PROGND)	; Start of variables address
FNDVAR:
	CALL	CPDEHL		; End of variable list table?
	JP	Z,CFEVAL	; Yes - Called from EVAL?
	LDB	RH0,RL1		; Get second byte of name
	SUBB	RH0,@R3		; Compare with name in list
	LDCTLB	RL4,FLAGS
	INC	R3		; Move on to first byte
	LDCTLB	FLAGS,RL4
	JP	NZ,FNTHR	; Different - Find another
	LDB	RH0,RH1		; Get first byte of name
	SUBB	RH0,@R3		; Compare with name in list
FNTHR:
	LDCTLB	RL4,FLAGS
	INC	R3		; Move on to LSB of value
	LDCTLB	FLAGS,RL4
	JP	Z,RETADR	; Found - Return address
	LDCTLB	RL4,FLAGS
	INC	R3		; <- Skip
	INC	R3		; <- over
	INC	R3		; <- F.P.
	INC	R3		; <- value
	LDCTLB	FLAGS,RL4
	JP	FNDVAR		; Keep looking
;
CFEVAL:
	POP	R3,@R15		; Restore code string address
	EX	@R15,R3		; Get return address
	PUSH	@R15,R2		; Save address of variable
	LD	R2,#FRMEVL	; Return address in EVAL
	CALL	CPDEHL		; Called from EVAL ?
	POP	R2,@R15		; Restore address of variable
	JP	Z,RETNUL	; Yes - Return null variable
	EX	@R15,R3		; Put back return
	PUSH	@R15,R3		; Save code string address
	PUSH	@R15,R1		; Save variable name
	LD	R1,#6		; 2 byte name plus 4 byte data
	LD	R3,(ARREND)	; End of arrays
	PUSH	@R15,R3		; Save end of arrays
	ADD	R3,R1		; Move up 6 bytes
	POP	R1,@R15		; Source address in BC
	PUSH	@R15,R3		; Save new end address
	CALL	MOVUP		; Move arrays up
	POP	R3,@R15		; Restore new end address
	LD	(ARREND),R3	; Set new end address
	LDB	RH3,RH1		; End of variables to HL
	LDB	RL3,RL1
	LD	(VAREND),R3	; Set new end address
;
ZEROLP:
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; Back through to zero variable
	LDCTLB	FLAGS,RL4
	LDB	@R3,#0		; Zero byte in variable
	CALL	CPDEHL		; Done them all?
	JP	NZ,ZEROLP	; No - Keep on going
	POP	R2,@R15		; Get variable name
	LDB	@R3,RL2		; Store second character
	LDCTLB	RL4,FLAGS
	INC	R3
	LDB	@R3,RH2		; Store first character
	INC	R3
	LDCTLB	FLAGS,RL4
RETADR:
	EX	R3,R2		; Address of variable in DE
	POP	R3,@R15		; Restore code string address
	RET
;
RETNUL:
	LDB	(FPEXP),RH0	; Set result to zero
	LD	R3,#ZERBYT	; Also set a null string
	EXB	RH3,RL3
	LD	(FPREG),R3	; Save for EVAL
	EXB	RH3,RL3
	POP	R3,@R15		; Restore code string address
	RET
;
SBSCPT:
	PUSH	@R15,R3		; Save code string address
	LD	R3,(LCRFLG)	; Locate/Create and Type
	EX	@R15,R3		; Save and get code string
	LDB	RH2,RH0		; Zero number of dimensions
SCPTLP:
	PUSH	@R15,R2		; Save number of dimensions
	PUSH	@R15,R1		; Save array name
	CALL	FPSINT		; Get subscript (0-32767)
	POP	R1,@R15		; Restore array name
	POP	R0,@R15		; Get number of dimensions
	LDCTLB	FLAGS,RL0
	EX	R3,R2
	EX	@R15,R3		; Save subscript value
	PUSH	@R15,R3		; Save LCRFLG and TYPE
	EX	R3,R2
	INCB	RH0,#1		; Count dimensions
	LDB	RH2,RH0		; Save in D
	LDB	RH0,@R3		; Get next byte in code string
	CPB	RH0,#','	; Comma (more to come)?
	JP	Z,SCPTLP	; Yes - More subscripts
	LD	RH4,#")"
	CALL	CHKSYN		; Make sure ")" follows
	LD	(NXTOPR),R3	; Save code string address
	POP	R3,@R15		; Get LCRFLG and TYPE
	LD	(LCRFLG),R3	; Restore Locate/create & type
	LDB	RL2,#0		; Flag not CSAVE* or CLOAD*
	PUSH	@R15,R2		; Save number of dimensions (D)
	JR	ARLDSV1		; Skip "PUSH HL" and "PUSH AF'
;
ARLDSV:
	PUSH	@R15,R3		; Save code string address
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; A = 00 , Flags set = Z,N
ARLDSV1:
	LD	R3,(VAREND)	; Start of arrays
	JR	FNDARY1		; Skip "ADD HL,DE"
FNDARY:
	ADD	R3,R2		; Move to next array start
FNDARY1:
	EX	R3,R2
	LD	R3,(ARREND)	; End of arrays
	EX	R3,R2		; Current array pointer
	CALL	CPDEHL		; End of arrays found?
	JP	Z,CREARY	; Yes - Create array
	LDB	RH0,@R3		; Get second byte of name
	CPB	RH0,RL1		; Compare with name given
	LDCTLB	RL4,FLAGS
	INC	R3		; Move on
	LDCTLB	FLAGS,RL4
	JP	NZ,NXTARY	; Different - Find next array
	LDB	RH0,@R3		; Get first byte of name
	CPB	RH0,RH1		; Compare with name given
NXTARY:
	LDCTLB	RL4,FLAGS
	INC	R3		; Move on
	LDB	RL2,@R3		; Get LSB of next array address
	INC	R3
	LDB	RH2,@R3		; Get MSB of next array address
	INC	R3
	LDCTLB	FLAGS,RL4
	JP	NZ,FNDARY	; Not found - Keep looking
	LDB	RH0,(LCRFLG)	; Found Locate or Create it?
	ORB	RH0,RH0
	RESFLG	C
	JP	NZ,DDERR	; Create - ?DD Error
	POP	R0,@R15		; Locate - Get number of dim'ns
	LDCTLB	FLAGS,RL0
	LDB	RH1,RH3		; BC Points to array dim'ns
	LDB	RL1,RL3
	JP	Z,POPHRT	; Jump if array load/save
	SUBB	RH0,@R3		; Same number of dimensions?
	JP	Z,FINDEL	; Yes - Find element
BSERR:
	LDB	RL2,#BS		; ?BS Error
	JP	ERROR		; Output error
;
CREARY:
	LD	R2,#4		; 4 Bytes per entry
	POP	R0,@R15		; Array to save or 0 dim'ns?
	LDCTLB	FLAGS,RL0
	JP	Z,FCERR		; Yes - ?FC Error
	LDB	@R3,RL1		; Save second byte of name
	LDCTLB	RL4,FLAGS
	INC	R3
	LDB	@R3,RH1		; Save first byte of name
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	RL1,RH0		; Number of dimensions to C
	CALL	CHKSTK		; Check if enough memory
	LDCTLB	RL4,FLAGS
	INC	R3		; Point to number of dimensions
	INC	R3
	LDCTLB	FLAGS,RL4
	LD	(CUROPR),R3	; Save address of pointer
	LDB	@R3,RL1		; Set number of dimensions
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	RH0,(LCRFLG)	; Locate of Create?
	RLCB	RH0,#1		; Carry set = Create
	LDB	RH0,RL1		; Get number of dimensions
CRARLP:
	LD	R1,#10+1	; Default dimension size 10
	JP	NC,DEFSIZ	; Locate - Set default size
	POP	R1,@R15		; Get specified dimension size
	LDCTLB	RL4,FLAGS
	INC	R1		; Include zero element
	LDCTLB	FLAGS,RL4
DEFSIZ:
	LDB	@R3,RL1		; Save LSB of dimension size
	LDCTLB	RL4,FLAGS
	INC	R3
	LDB	@R3,RH1		; Save MSB of dimension size
	INC	R3
	LDCTLB	FLAGS,RL4
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Save num' of dim'ns an status
	PUSH	@R15,R3		; Save address of dim'n size
	CALL	MLDEBC		; Multiply DE by BC to find
	EX	R3,R2		; amount of mem needed (to DE)
	POP	R3,@R15		; Restore address of dimension
	POP	R0,@R15		; Restore number of dimensions
	LDCTLB	FLAGS,RL0
	DECB	RH0,#1		; Count them
	JP	NZ,CRARLP	; Do next dimension if more
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Save locate/create flag
	LDB	RH1,RH2		; MSB of memory needed
	LDB	RL1,RL2		; LSB of memory needed
	EX	R3,R2
	ADD	R3,R2		; Add bytes to array start
	JP	C,OMERR		; Too big - Error
	CALL	ENFMEM		; See if enough memory
	LD	(ARREND),R3	; Save new end of array
;
ZERARY:
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; Back through array data
	LDCTLB	FLAGS,RL4
	LDB	@R3,#0		; Set array element to zero
	CALL	CPDEHL		; All elements zeroed?
	JP	NZ,ZERARY	; No - Keep on going
	LDCTLB	RL4,FLAGS
	INC	R1		; Number of bytes + 1
	LDCTLB	FLAGS,RL4
	LDB	RH2,RH0		; A=0
	LD	R3,(CUROPR)	; Get address of array
	LDB	RL2,@R3		; Number of dimensions
	EX	R3,R2		; To HL
	ADD	R3,R3		; Two bytes per dimension size
	ADD	R3,R1		; Add number of bytes
	EX	R3,R2		; Bytes needed to DE
	DEC	R3,#1
	DEC	R3,#1
	LDB	@R3,RL2		; Save LSB of bytes needed
	INC	R3
	LDB	@R3,RH2		; Save MSB of bytes needed
	INC	R3
	POP	R0,@R15		; Locate / Create?
	LDCTLB	FLAGS,RL0
	JP	C,ENDDIM	; A is 0 , End if create
FINDEL:
	LDB	RH1,RH0		; Find array element
	LDB	RL1,RH0
	LDB	RH0,@R3		; Number of dimensions
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	JR	FNDELP1		; Skip "POP HL"
FNDELP:
	POP	R3,@R15		; Address of next dim' size
FNDELP1:
	LDB	RL2,@R3		; Get LSB of dim'n size
	LDCTLB	RL4,FLAGS
	INC	R3
	LDB	RH2,@R3		; Get MSB of dim'n size
	INC	R3
	LDCTLB	FLAGS,RL4
	EX	@R15,R3		; Save address - Get index
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Save number of dim'ns
	CALL	CPDEHL		; Dimension too large?
	JP	NC,BSERR	; Yes - ?BS Error
	PUSH	@R15,R3		; Save index
	CALL	MLDEBC		; Multiply previous by size
	POP	R2,@R15		; Index supplied to DE
	ADD	R3,R2		; Add index to pointer
	POP	R0,@R15		; Number of dimensions
	LDCTLB	FLAGS,RL0
	DECB	RH0,#1		; Count them
	LDB	RH1,RH3		; MSB of pointer
	LDB	RL1,RL3		; LSB of pointer
	JP	NZ,FNDELP	; More - Keep going
	ADD	R3,R3		; 4 Bytes per element
	ADD	R3,R3
	POP	R1,@R15		; Start of array
	ADD	R3,R1		; Point to element
	EX	R3,R2		; Address of element to DE
ENDDIM:
	LD	R3,(NXTOPR)	; Got code string address
	RET
;
FRE:
	LD	R3,(ARREND)	; Start of free memory
	EX	R3,R2		; To DE
	LD	R3,#0		; End of free memory
	ADD	R3,R15		; Current stack value
	LDB	RH0,(TYPE)	; Dummy argument type
	ORB	RH0,RH0
	RESFLG	C
	JP	Z,FRENUM	; Numeric - Free variable space
	CALL	GSTRCU		; Current string to pool
	CALL	GARBGE		; Garbage collection
	LD	R3,(STRSPC)	; Bottom of string space in use
	EX	R3,R2		; To DE
	LD	R3,(STRBOT)	; Bottom of string space
FRENUM:
	LDB	RH0,RL3		; Get LSB of end
	SUBB	RH0,RL2		; Subtract LSB of beginning
	LDB	RL1,RH0		; Save difference if C
	LDB	RH0,RH3		; Get MSB of end
	SBCB	RH0,RH2		; Subtract MSB of beginning
ACPASS:
	LDB	RH1,RL1		; Return integer AC
ABPASS:
	LDB	RH2,RH1		; Return integer AB
	LDB	RL2,#0		; Point to type
	LD	R3,#TYPE
	LDB	@R3,RL2		; Set type to numeric
	LDB	RH1,#80H+16	; 16 bit integer
	JP	RETINT		; Return the integr
;
POS:
	LDB	RH0,(CURPOS)	; Get cursor position
PASSA:
	LDB	RH1,RH0		; Put A into AB
	XORB	RH0,RH0
	RESFLG	C		; Zero A
	JP	ABPASS		; Return integer AB
;
DEF:
	CALL	CHEKFN		; Get "FN" and name
	CALL	IDTEST		; Test for illegal direct
	LD	R1,#DATA	; To get next statement
	PUSH	@R15,R1		; Save address for RETurn
	PUSH	@R15,R2		; Save address of function ptr
	LD	RH4,#"("
	CALL	CHKSYN		; Make sure "(" follows
	CALL	GETVAR		; Get argument variable name
	PUSH	@R15,R3		; Save code string address
	EX	R3,R2		; Argument address to HL
	LDCTLB	RL4,FLAGS
	DEC	R3,#1
	LDB	RH2,@R3		; Get first byte of arg name
	DEC	R3,#1
	LDCTLB	FLAGS,RL4
	LDB	RL2,@R3		; Get second byte of arg name
	POP	R3,@R15		; Restore code string address
	CALL	TSTNUM		; Make sure numeric argument
	LD	RH4,#")"
	CALL	CHKSYN		; Make sure ")" follows
	LD	RH4,#ZEQUAL	; "=" token
	CALL	CHKSYN		; Make sure "=" follows
	LDB	RH1,RH3		; Code string address to BC
	LDB	RL1,RL3
	EX	@R15,R3		; Save code str , Get FN ptr
	LDB	@R3,RL1		; Save LSB of FN code string
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	@R3,RH1		; Save MSB of FN code string
	JP	SVSTAD		; Save address and do function
;
DOFN:
	CALL	CHEKFN		; Make sure FN follows
	PUSH	@R15,R2		; Save function pointer address
	CALL	EVLPAR		; Evaluate expression in "()"
	CALL	TSTNUM		; Make sure numeric result
	EX	@R15,R3		; Save code str , Get FN ptr
	LDB	RL2,@R3		; Get LSB of FN code string
	LDCTLB	RL4,FLAGS
	INC	R3
	LDB	RH2,@R3		; Get MSB of FN code string
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	RH0,RH2		; And function DEFined?
	ORB	RH0,RL2
	RESFLG	C
	JP	Z,UFERR		; No - ?UF Error
	LDB	RH0,@R3		; Get LSB of argument address
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	RH3,@R3		; Get MSB of argument address
	LDB	RL3,RH0		; HL = Arg variable address
	PUSH	@R15,R3		; Save it
	LD	R3,(FNRGNM)	; Get old argument name
	EX	@R15,R3;	; Save old , Get new
	LD	(FNRGNM),R3	; Set new argument name
	LD	R3,(FNARG+2)	; Get LSB,NLSB of old arg value
	PUSH	@R15,R3		; Save it
	LD	R3,(FNARG)	; Get MSB,EXP of old arg value
	PUSH	@R15,R3		; Save it
	LD	R3,#FNARG	; HL = Value of argument
	PUSH	@R15,R2		; Save FN code string address
	CALL	FPTHL		; Move FPREG to argument
	POP	R3,@R15		; Get FN code string address
	CALL	GETNUM		; Get value from function
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; DEC 'cos GETCHR INCs
	LDCTLB	FLAGS,RL4
	CALL	GETCHR		; Get next character
	JP	NZ,SNERR	; Bad character in FN - Error
	POP	R3,@R15		; Get MSB,EXP of old arg
	LD	(FNARG),R3	; Restore it
	POP	R3,@R15		; Get LSB,NLSB of old arg
	LD	(FNARG+2),R3	; Restore it
	POP	R3,@R15		; Get name of old arg
	LD	(FNRGNM),R3	; Restore it
	POP	R3,@R15		; Restore code string address
	RET
;
IDTEST:
	PUSH	@R15,R3		; Save code string address
	LD	R3,(LINEAT)	; Get current line number
	LDCTLB	RL4,FLAGS
	INC	R3		; -1 means direct statement
	LDCTLB	FLAGS,RL4
	LDB	RH0,RH3
	ORB	RH0,RL3
	RESFLG	C
	POP	R3,@R15		; Restore code string address
	RET	NZ		; Return if in program
	LDB	RL2,#ID		; ?ID Error
	JP	ERROR
;
CHEKFN:
	LD	RH4,#ZFN	; "FN" token
	CALL	CHKSYN		; Make sure FN follows
	LDB	RH0,#80H
	LDB	(FORFLG),RH0	; Flag FN name to find
	ORB	RH0,@R3
	RESFLG	C		; FN name has bit 7 set
	LDB	RH1,RH0		; in first byte of name
	CALL	GTFNAM		; Get FN name
	JP	TSTNUM		; Make sure numeric function
;
STR:
	CALL	TSTNUM		; Make sure it's a number
	CALL	NUMASC		; Turn number into text
STR1:
	CALL	CRTST		; Create string entry for it
	CALL	GSTRCU		; Current string to pool
	LD	R1,#TOPOOL	; Save in string pool
	PUSH	@R15,R1		; Save address on stack
;
SAVSTR:
	LDB	RH0,@R3		; Get string length
	LDCTLB	RL4,FLAGS
	INC	R3
	INC	R3
	LDCTLB	FLAGS,RL4
	PUSH	@R15,R3		; Save pointer to string
	CALL	TESTR		; See if enough string space
	POP	R3,@R15		; Restore pointer to string
	LDB	RL1,@R3		; Get LSB of address
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	RH1,@R3		; Get MSB of address
	CALL	CRTMST		; Create string entry
	PUSH	@R15,R3		; Save pointer to MSB of addr
	LDB	RL3,RH0		; Length of string
	CALL	TOSTRA		; Move to string area
	POP	R2,@R15		; Restore pointer to MSB
	RET
;
MKTMST:
	CALL	TESTR		; See if enough string space
CRTMST:
	LD	R3,#TMPSTR	; Temporary string
	PUSH	@R15,R3		; Save it
	LDB	@R3,RH0		; Save length of string
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
SVSTAD:
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	@R3,RL2		; Save LSB of address
	INC	R3
	LDB	@R3,RH2		; Save MSB of address
	POP	R3,@R15		; Restore pointer
	RET
;
CRTST:
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; DEC - INCed after
	LDCTLB	FLAGS,RL4
QTSTR:
	LDB	RH1,#'"'	; Terminating quote
	LDB	RH2,RH1		; Quote to D
DTSTR:
	PUSH	@R15,R3		; Save start
	LDB	RL1,#-1		; Set counter to -1
QTSTLP:
	LDCTLB	RL4,FLAGS
	INC	R3		; Move on
	LDCTLB	FLAGS,RL4
	LDB	RH0,@R3		; Get byte
	INCB	RL1,#1		; Count bytes
	ORB	RH0,RH0
;	RESFLG	C		; End of line?
	JP	Z,CRTSTE	; Yes - Create string entry
	CPB	RH0,RH2		; Terminator D found?
	JP	Z,CRTSTE	; Yes - Create string entry
	CPB	RH0,RH1		; Terminator B found?
	JP	NZ,QTSTLP	; No - Keep looking
CRTSTE:
	CPB	RH0,#'"'	; End with '"'?
	JR	NZ,CRTSTE1
	CALL	GETCHR		; Yes - Get next character
CRTSTE1:
	EX	@R15,R3		; Starting quote
	LDCTLB	RL4,FLAGS
	INC	R3		; First byte of string
	LDCTLB	FLAGS,RL4
	EX	R3,R2		; To DE
	LDB	RH0,RL1		; Get length
	CALL	CRTMST		; Create string entry
TSTOPL:
	LD	R2,#TMPSTR	; Temporary string
	LD	R3,(TMSTPT)	; Temporary string pool pointer
	EXB	RH3,RL3
	LD	(FPREG),R3	; Save address of string ptr
	EXB	RH3,RL3
	LDB	RH0,#1
	LDB	(TYPE),RH0	; Set type to string
	CALL	DETHL4		; Move string to pool
	CALL	CPDEHL		; Out of string pool?
	LD	(TMSTPT),R3	; Save new pointer
	POP	R3,@R15		; Restore code string address
	LDB	RH0,@R3		; Get next code byte
	RET	NZ		; Return if pool OK
	LDB	RL2,#ST		; ?ST Error
	JP	ERROR		; String pool overflow
;
PRNUMS:
	LDCTLB	RL4,FLAGS
	INC	R3		; Skip leading space
	LDCTLB	FLAGS,RL4
PRS:
	CALL	CRTST		; Create string entry for it
PRS1:
	CALL	GSTRCU		; Current string to pool
	CALL	LOADFP		; Move string block to BCDE
	INCB	RL2,#1		; Length + 1
PRSLP:
	DECB	RL2,#1		; Count characters
	RET	Z		; End of string
	LDB	RH0,@R1		; Get byte to output
	CALL	OUTC		; Output character in A
	CPB	RH0,#CR		; Return?
	JR	NZ,PRSLP1
	CALL	DONULL		; Yes - Do nulls
PRSLP1:
	LDCTLB	RL4,FLAGS
	INC	R1		; Next byte in string
	LDCTLB	FLAGS,RL4
	JP	PRSLP		; More characters to output
;
TESTR:
	ORB	RH0,RH0
	RESFLG	C		; Test if enough room
	JR	GRBDON1		; No garbage collection done
GRBDON:
	POP	R0,@R15		; Garbage collection done
	LDCTLB	FLAGS,RL0
GRBDON1:
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Save status
	LD	R3,(STRSPC)	; Bottom of string space in use
	EX	R3,R2		; To DE
	LD	R3,(STRBOT)	; Bottom of string area
	LDCTLB	RL4,FLAGS
	COMB	RH0		; Negate length (Top down)
	LDCTLB	FLAGS,RL4
	LDB	RL1,RH0		; -Length to BC
	LDB	RH1,#-1		; BC = -ve length of string
	ADD	R3,R1		; Add to bottom of space in use
	LDCTLB	RL4,FLAGS
	INC	R3		; Plus one for 2's complement
	LDCTLB	FLAGS,RL4
	CALL	CPDEHL		; Below string RAM area?
	JP	C,TESTOS	; Tidy up if not done else err
	LD	(STRBOT),R3	; Save new bottom of area
	LDCTLB	RL4,FLAGS
	INC	R3		; Point to first byte of string
	LDCTLB	FLAGS,RL4
	EX	R3,R2		; Address to DE
POPAF:
	POP	R0,@R15		; Throw away status push
	LDCTLB	FLAGS,RL0
	RET
;
TESTOS:
	POP	R0,@R15		; Garbage collect been done?
	LDCTLB	FLAGS,RL0
	LDB	RL2,#OS		; ?OS Error
	JP	Z,ERROR		; Yes - Not enough string apace
	CPB	RH0,RH0		; Flag garbage collect done
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Save status
	LD	R1,#GRBDON	; Garbage collection done
	PUSH	@R15,R1		; Save for RETurn
GARBGE:
	LD	R3,(LSTRAM)	; Get end of RAM pointer
GARBLP:
	LD	(STRBOT),R3	; Reset string pointer
	LD	R3,#0
	PUSH	@R15,R3		; Flag no string found
	LD	R3,(STRSPC)	; Get bottom of string space
	PUSH	@R15,R3		; Save bottom of string space
	LD	R3,#TMSTPL	; Temporary string pool
GRBLP:
	EX	R3,R2
	LD	R3,(TMSTPT)	; Temporary string pool pointer
	EX	R3,R2
	CALL	CPDEHL		; Temporary string pool done?
	LD	R1,#GRBLP	; Loop until string pool done
	JP	NZ,STPOOL	; No - See if in string area
	LD	R3,(PROGND)	; Start of simple variables
SMPVAR:
	EX	R3,R2
	LD	R3,(VAREND)	; End of simple variables
	EX	R3,R2
	CALL	CPDEHL		; All simple strings done?
	JP	Z,ARRLP		; Yes - Do string arrays
	LDB	RH0,@R3		; Get type of variable
	LDCTLB	RL4,FLAGS
	INC	R3
	INC	R3
	LDCTLB	FLAGS,RL4
	ORB	RH0,RH0
	RESFLG	C		; "S" flag set if string
	CALL	STRADD		; See if string in string area
	JP	SMPVAR		; Loop until simple ones done
;
GNXARY:
	POP	R1,@R15		; Scrap address of this array
ARRLP:
	EX	R3,R2
	LD	R3,(ARREND)	; End of string arrays
	EX	R3,R2
	CALL	CPDEHL		; All string arrays done?
	JP	Z,SCNEND	; Yes - Move string if found
	CALL	LOADFP		; Get array name to BCDE
	LDB	RH0,RL2		; Get type of array
	PUSH	@R15,R3		; Save address of num of dim'ns
	ADD	R3,R1		; Start of next array
	ORB	RH0,RH0
	RESFLG	C		; Test type of array
	JP	PL,GNXARY	; Numeric array - Ignore it
	LD	(CUROPR),R3	; Save address of next array
	POP	R3,@R15		; Get address of num of dim'ns
	LDB	RL1,@R3		; BC = Number of dimensions
	LDB	RH1,#0
	ADD	R3,R1		; Two bytes per dimension size
	ADD	R3,R1
	LDCTLB	RL4,FLAGS
	INC	R3		; Plus one for number of dim'ns
	LDCTLB	FLAGS,RL4
GRBARY:
	EX	R3,R2
	LD	R3,(CUROPR)	; Get address of next array
	EX	R3,R2
	CALL	CPDEHL		; Is this array finished?
	JP	Z,ARRLP		; Yes - Get next one
	LD	R1,#GRBARY	; Loop until array all done
STPOOL:
	PUSH	@R15,R1		; Save return address
	ORB	RH0,#80H	; Flag string type
	RESFLG	C
STRADD:
	LDB	RH0,@R3		; Get string length
	LDCTLB	RL4,FLAGS
	INC	R3
	INC	R3
	LDB	RL2,@R3		; Get LSB of string address
	INC	R3
	LDB	RH2,@R3		; Get MSB of string address
	INC	R3
	LDCTLB	FLAGS,RL4
	RET	PL		; Not a string - Return
	ORB	RH0,RH0
	RESFLG	C		; Set flags on string length
	RET	Z		; Null string - Return
	LDB	RH1,RH3		; Save variable pointer
	LDB	RL1,RL3
	LD	R3,(STRBOT)	; Bottom of new area
	CALL	CPDEHL		; String been done?
	LDB	RH3,RH1		; Restore variable pointer
	LDB	RL3,RL1
	RET	C		; String done - Ignore
	POP	R3,@R15		; Return address
	EX	@R15,R3		; Lowest available string area
	CALL	CPDEHL		; String within string area?
	EX	@R15,R3		; Lowest available string area
	PUSH	@R15,R3		; Re-save return address
	LDB	RH3,RH1		; Restore variable pointer
	LDB	RL3,RL1
	RET	NC		; Outside string area - Ignore
	POP	R1,@R15		; Get return , Throw 2 away
	POP	R0,@R15		;
	LDCTLB	FLAGS,RL0
	POP	R0,@R15		;
	LDCTLB	FLAGS,RL0
	PUSH	@R15,R3		; Save variable pointer
	PUSH	@R15,R2		; Save address of current
	PUSH	@R15,R1		; Put back return address
	RET			; Go to it
;
SCNEND:
	POP	R2,@R15		; Addresses of strings
	POP	R3,@R15		;
	LDB	RH0,RL3		; HL = 0 if no more to do
	ORB	RH0,RH3
	RESFLG	C
	RET	Z		; No more to do - Return
	LDCTLB	RL4,FLAGS
	DEC	R3,#1
	LDB	RH1,@R3		; MSB of address of string
	DEC	R3,#1
	LDB	RL1,@R3		; LSB of address of string
	PUSH	@R15,R3		; Save variable address
	DEC	R3,#1
	DEC	R3,#1
	LDCTLB	FLAGS,RL4
	LDB	RL3,@R3		; HL = Length of string
	LDB	RH3,#0
	ADD	R3,R1		; Address of end of string+1
	LDB	RH2,RH1		; String address to DE
	LDB	RL2,RL1
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; Last byte in string
	LDCTLB	FLAGS,RL4
	LDB	RH1,RH3		; Address to BC
	LDB	RL1,RL3
	LD	R3,(STRBOT)	; Current bottom of string area
	CALL	MOVSTR		; Move string to new address
	POP	R3,@R15		; Restore variable address
	LDB	@R3,RL1		; Save new LSB of address
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	@R3,RH1		; Save new MSB of address
	LDB	RL3,RL1		; Next string area+1 to HL
	LDB	RH3,RH1
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; Next string area address
	LDCTLB	FLAGS,RL4
	JP	GARBLP		; Look for more strings
;
CONCAT:
	PUSH	@R15,R1		; Save prec' opr & code string
	PUSH	@R15,R3		;
	LD	R3,(FPREG)	; Get first string
	EXB	RH3,RL3
	EX	@R15,R3		; Save first string
	CALL	OPRND		; Get second string
	EX	@R15,R3		; Restore first string
	CALL	TSTSTR		; Make sure it's a string
	LDB	RH0,@R3		; Get length of second string
	PUSH	@R15,R3		; Save first string
	LD	R3,(FPREG)	; Get second string
	EXB	RH3,RL3
	PUSH	@R15,R3		; Save second string
	ADDB	RH0,@R3		; Add length of second string
	LDB	RL2,#LS		; ?LS Error
	JP	C,ERROR		; String too long - Error
	CALL	MKTMST		; Make temporary string
	POP	R2,@R15		; Get second string to DE
	CALL	GSTRDE		; Move to string pool if needed
	EX	@R15,R3		; Get first string
	CALL	GSTRHL		; Move to string pool if needed
	PUSH	@R15,R3		; Save first string
	LD	R3,(TMPSTR+2)	; Temporary string address
	EX	R3,R2		; To DE
	CALL	SSTSA		; First string to string area
	CALL	SSTSA		; Second string to string area
	LD	R3,#EVAL2	; Return to evaluation loop
	EX	@R15,R3		; Save return,get code string
	PUSH	@R15,R3		; Save code string address
	JP	TSTOPL		; To temporary string to pool
;
SSTSA:
	POP	R3,@R15		; Return address
	EX	@R15,R3		; Get string block,save return
	LDB	RH0,@R3		; Get length of string
	LDCTLB	RL4,FLAGS
	INC	R3
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	RL1,@R3		; Get LSB of string address
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	RH1,@R3		; Get MSB of string address
	LDB	RL3,RH0		; Length to L
TOSTRA:
	INCB	RL3,#1		; INC - DECed after
TSALP:
	DECB	RL3,#1		; Count bytes moved
	RET	Z		; End of string - Return
	LDB	RH0,@R1		; Get source
	LDB	@R2,RH0		; Save destination
	LDCTLB	RL4,FLAGS
	INC	R1		; Next source
	INC	R2		; Next destination
	LDCTLB	FLAGS,RL4
	JP	TSALP		; Loop until string moved
;
GETSTR:
	CALL	TSTSTR		; Make sure it's a string
GSTRCU:
	LD	R3,(FPREG)	; Get current string
	EXB	RH3,RL3
GSTRHL:
	EX	R3,R2		; Save DE
GSTRDE:
	CALL	BAKTMP		; Was it last tmp-str?
	EX	R3,R2		; Restore DE
	RET	NZ		; No - Return
	PUSH	@R15,R2		; Save string
	LDB	RH2,RH1		; String block address to DE
	LDB	RL2,RL1
	LDCTLB	RL4,FLAGS
	DEC	R2,#1		; Point to length
	LDCTLB	FLAGS,RL4
	LDB	RL1,@R3		; Get string length
	LD	R3,(STRBOT)	; Current bottom of string area
	CALL	CPDEHL		; Last one in string area?
	JP	NZ,POPHL	; No - Return
	LDB	RH1,RH0		; Clear B (A=0)
	ADD	R3,R1		; Remove string from str' area
	LD	(STRBOT),R3	; Save new bottom of str' area
POPHL:
	POP	R3,@R15		; Restore string
	RET
;
BAKTMP:
	LD	R3,(TMSTPT)	; Get temporary string pool top
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; Back
	LDB	RH1,@R3		; Get MSB of address
	DEC	R3,#1		; Back
	LDB	RL1,@R3		; Get LSB of address
	DEC	R3,#1		; Back
	DEC	R3,#1		; Back
	LDCTLB	FLAGS,RL4
	CALL	CPDEHL		; String last in string pool?
	RET	NZ		; Yes - Leave it
	LD	(TMSTPT),R3	; Save new string pool top
	RET
;
LEN:
	LD	R1,#PASSA	; To return integer A
	PUSH	@R15,R1		; Save address
GETLEN:
	CALL	GETSTR		; Get string and its length
	XORB	RH0,RH0
	RESFLG	C
	LDB	RH2,RH0		; Clear D
	LDB	(TYPE),RH0	; Set type to numeric
	LDB	RH0,@R3		; Get length of string
	ORB	RH0,RH0
	RESFLG	C		; Set status flags
	RET
;
ASC:
	LD	R1,#PASSA	; To return integer A
	PUSH	@R15,R1		; Save address
GTFLNM:
	CALL	GETLEN		; Get length of string
	JP	Z,FCERR		; Null string - Error
	LDCTLB	RL4,FLAGS
	INC	R3
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	RL2,@R3		; Get LSB of address
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	RH2,@R3		; Get MSB of address
	LDB	RH0,@R2		; Get first byte of string
	RET
;
CHR:
	LDB	RH0,#1		; One character string
	CALL	MKTMST		; Make a temporary string
	CALL	MAKINT		; Make it integer A
	LD	R3,(TMPSTR+2)	; Get address of string
	EXB	RH3,RL3		; @@@
	LDB	@R3,RL2		; Save character
TOPOOL:
	POP	R1,@R15		; Clean up stack
	JP	TSTOPL		; Temporary string to pool
;
LEFT:
	CALL	LFRGNM		; Get number and ending ")"
	XORB	RH0,RH0
	RESFLG	C		; Start at first byte in string
RIGHT1:
	EX	@R15,R3		; Save code string,Get string
	LDB	RL1,RH0		; Starting position in string
MID1:
	PUSH	@R15,R3		; Save string block address
	LDB	RH0,@R3		; Get length of string
	CPB	RH0,RH1		; Compare with number given
	JP	C,ALLFOL	; All following bytes required
	LDB	RH0,RH1		; Get new length
	JR	ALLFOL1		; Skip "LD C,0"
ALLFOL:
	LDB	RL1,#0		; First byte of string
ALLFOL1:
	PUSH	@R15,R1		; Save position in string
	CALL	TESTR		; See if enough string space
	POP	R1,@R15		; Get position in string
	POP	R3,@R15		; Restore string block address
	PUSH	@R15,R3		; And re-save it
	LDCTLB	RL4,FLAGS
	INC	R3
	INC	R3
	LDB	RH1,@R3		; Get LSB of address
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	RH3,@R3		; Get MSB of address
	LDB	RL3,RH1		; HL = address of string
	LDB	RH1,#0		; BC = starting address
	ADD	R3,R1		; Point to that byte
	LDB	RH1,RH3		; BC = source string
	LDB	RL1,RL3
	CALL	CRTMST		; Create a string entry
	LDB	RL3,RH0		; Length of new string
	CALL	TOSTRA		; Move string to string area
	POP	R2,@R15		; Clear stack
	CALL	GSTRDE		; Move to string pool if needed
	JP	TSTOPL		; Temporary string to pool
;
RIGHT:
	CALL	LFRGNM		; Get number and ending ")"
	POP	R2,@R15		; Get string length
	PUSH	@R15,R2		; And re-save
	LDB	RH0,@R2		; Get length
	SUBB	RH0,RH1		; Move back N bytes
	JP	RIGHT1		; Go and get sub-string
;
MID:
	EX	R3,R2		; Get code string address
	LDB	RH0,@R3		; Get next byte ',' or ")"
	CALL	MIDNUM		; Get number supplied
	INCB	RH1,#1		; Is it character zero?
	DECB	RH1,#1
	JP	Z,FCERR		; Yes - Error
	PUSH	@R15,R1		; Save starting position
	LDB	RL2,#255	; All of string
	CPB	RH0,#')'	; Any length given?
	JP	Z,RSTSTR	; No - Rest of string
	LD	RH4,#','
	CALL	CHKSYN		; Make sure ',' follows
	CALL	GETINT		; Get integer 0-255
RSTSTR:
	LD	RH4,#")"
	CALL	CHKSYN		; Make sure ")" follows
	POP	R0,@R15		; Restore starting position
	LDCTLB	FLAGS,RL0
	EX	@R15,R3		; Get string,8ave code string
	LD	R1,#MID1	; Continuation of MID$ routine
	PUSH	@R15,R1		; Save for return
	DECB	RH0,#1		; Starting position-1
	CPB	RH0,@R3		; Compare with length
	LDB	RH1,#0		; Zero bytes length
	RET	NC		; Null string if start past end
	LDB	RL1,RH0		; Save starting position-1
	LDB	RH0,@R3		; Get length of string
	SUBB	RH0,RL1		; Subtract start
	CPB	RH0,RL2		; Enough string for it?
	LDB	RH1,RH0		; Save maximum length available
	RET	C		; Truncate string if needed
	LDB	RH1,RL2		; Set specified length
	RET			; Go and create string
;
VAL:
	CALL	GETLEN		; Get length of string
	JP	Z,RESZER	; Result zero
	LDB	RL2,RH0		; Save length
	LDCTLB	RL4,FLAGS
	INC	R3
	INC	R3
	LDB	RH0,@R3		; Get LSB of address
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	RH3,@R3		; Get MSB of address
	LDB	RL3,RH0		; HL = String address
	PUSH	@R15,R3		; Save string address
	ADD	R3,R2
	LDB	RH1,@R3		; Get end of string+1 byte
	LDB	@R3,RH2		; Zero it to terminate
	EX	@R15,R3		; Save string end,get start
	PUSH	@R15,R1		; Save end+1 byte
	LDB	RH0,@R3		; Get starting byte
	CPB	RH0,#'$'	; Hex number indicated? [function added]
	JP	NZ,VAL1
	CALL	HEXTFP		; Convert Hex to FPREG
	JR	VAL3
VAL1:
	CPB	RH0,#'%'	; Binary number indicated? [function added]
	JP	NZ,VAL2
	CALL	BINTFP		; Convert Bin to FPREG
	JR	VAL3
VAL2:
	CALL	ASCTFP		; Convert ASCII string to FP
VAL3:
	POP	R1,@R15		; Restore end+1 byte
	POP	R3,@R15		; Restore end+1 address
	LDB	@R3,RH1		; Put back original byte
	RET
;
LFRGNM:
	EX	R3,R2		; Code string address to HL
	LD	RH4,#")"
	CALL	CHKSYN		; Make sure ")" follows
MIDNUM:
	POP	R1,@R15		; Get return address
	POP	R2,@R15		; Get number supplied
	PUSH	@R15,R1		; Re-save return address
	LDB	RH1,RL2		; Number to B
	RET
;
INP:
	CALL	MAKINT		; Make it integer A
	LDB	(INPORT),RH0	; Set input port
	CALL	INPSUB		; Get input from port
	JP	PASSA		; Return integer A
;
POUT:
	CALL	SETIO		; Set up port number
	JP	OUTSUB		; Output data and return
;
WAIT:
	CALL	SETIO		; Set up port number
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Save AND mask
	LDB	RL2,#0		; Assume zero if none given
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; DEC 'cos GETCHR INCs
	LDCTLB	FLAGS,RL4
	CALL	GETCHR		; Get next character
	JP	Z,NOXOR		; No XOR byte given
	LD	RH4,#','
	CALL	CHKSYN		; Make sure ',' follows
	CALL	GETINT		; Get integer 0-255 to XOR with
NOXOR:
	POP	R1,@R15		; Restore AND mask
WAITLP:
	CALL	INPSUB		; Get input
	XORB	RH0,RL2
	RESFLG	C		; Flip selected bits
	ANDB	RH0,RH1
	RESFLG	C		; Result non-zero?
	JP	Z,WAITLP	; No = keep waiting
	RET
;
SETIO:
	CALL	GETINT		; Get integer 0-255
	LDB	(INPORT),RH0	; Set input port
	LDB	(OTPORT),RH0	; Set output port
	LD	RH4,#','
	CALL	CHKSYN		; Make sure ',' follows
	JP	GETINT		; Get integer 0-255 and return
;
FNDNUM:
	CALL	GETCHR		; Get next character
GETINT:
	CALL	GETNUM		; Get a number from 0 to 255
MAKINT:
	CALL	DEPINT		; Make sure value 0 - 255
	LDB	RH0,RH2		; Get MSB of number
	ORB	RH0,RH0
	RESFLG	C		; Zero?
	JP	NZ,FCERR	; No - Error
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; DEC 'cos GETCHR INCs
	LDCTLB	FLAGS,RL4
	CALL	GETCHR		; Get next character
	LDB	RH0,RL2		; Get number to A
	RET
;
PEEK:
	CALL	DEINT		; Get memory address
	LDB	RH0,@R2		; Get byte in memory
	JP	PASSA		; Return integer A
;
POKE:
	CALL	GETNUM		; Get memory address
	CALL	DEINT		; Get integer -32768 to 3276
	PUSH	@R15,R2		; Save memory address
	LD	RH4,#','
	CALL	CHKSYN		; Make sure ',' follows
	CALL	GETINT		; Get integer 0-255
	POP	R2,@R15		; Restore memory address
	LDB	@R2,RH0		; Load it into memory
	RET
;
ROUND:
	LD	R3,#HALF	; Add 0.5 to FPREG
ADDPHL:
	CALL	LOADFP		; Load FP at (HL) to BCDE
	JP	FPADD		; Add BCDE to FPREG
;
SUBPHL:
	CALL	LOADFP		; FPREG = -FPREG + number at HL
	JR	SUBCDE		; Skip "POP BC" and "POP DE"
PSUB:
	POP	R1,@R15		; Get FP number from stack
	POP	R2,@R15
SUBCDE:
	CALL	INVSGN		; Negate FPREG
FPADD:
	LDB	RH0,RH1		; Get FP exponent
	ORB	RH0,RH0
	RESFLG	C		; Is number zero?
	RET	Z		; Yes - Nothing to add
	LDB	RH0,(FPEXP)	; Get FPREG exponent
	ORB	RH0,RH0
	RESFLG	C		; Is this number zero?
	JP	Z,FPBCDE	; Yes - Move BCDE to FPREG
	SUBB	RH0,RH1		; BCDE number larger?
	JP	NC,NOSWAP	; No - Don't swap them
	LDCTLB	RL4,FLAGS
	COMB	RH0		; Two's complement
	LDCTLB	FLAGS,RL4
	INCB	RH0,#1		; FP exponent
	EX	R3,R2
	CALL	STAKFP		; Put FPREG on stack
	EX	R3,R2
	CALL	FPBCDE		; Move BCDE to FPREG
	POP	R1,@R15		; Restore number from stack
	POP	R2,@R15
NOSWAP:
	CPB	RH0,#24+1	; Second number insignificant?
	RET	NC		; Yes - First number is result
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Save number of bits to scale
	CALL	SIGNS		; Set MSBs & sign of result
	LDB	RH3,RH0		; Save sign of result
	POP	R0,@R15		; Restore scaling factor
	LDCTLB	FLAGS,RL0
	CALL	SCALE		; Scale BCDE to same exponent
	ORB	RH0,RH3
	RESFLG	C		; Result to be positive?
	LD	R3,#FPREG	; Point to FPREG
	JP	PL,MINCDE	; No - Subtract FPREG from CDE
	CALL	PLUCDE		; Add FPREG to CDE
	JP	NC,RONDUP	; No overflow - Round it up
	LDCTLB	RL4,FLAGS
	INC	R3		; Point to exponent
	LDCTLB	FLAGS,RL4
	INCB	@R3,#1		; Increment it
	JP	Z,OVERR		; Number overflowed - Error
	LDB	RL3,#1		; 1 bit to shift right
	CALL	SHRT1		; Shift result right
	JP	RONDUP		; Round it up
;
MINCDE:
	XORB	RH0,RH0
	RESFLG	C		; Clear A and carry
	SUBB	RH0,RH1		; Negate exponent
	LDB	RH1,RH0		; Re-save exponent
	LDB	RH0,@R3		; Get LSB of FPREG
	SBCB	RH0,RL2		; Subtract LSB of BCDE
	LDB	RL2,RH0		; Save LSB of BCDE
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	RH0,@R3		; Get NMSB of FPREG
	SBCB	RH0,RH2		; Subtract NMSB of BCDE
	LDB	RH2,RH0		; Save NMSB of BCDE
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	RH0,@R3		; Get MSB of FPREG
	SBCB	RH0,RL1		; Subtract MSB of BCDE
	LDB	RL1,RH0		; Save MSB of BCDE
CONPOS:
	JR	NC,BNORM
	CALL	COMPL		; Overflow - Make it positive
;
BNORM:
	LDB	RL3,RH1		; L = Exponent
	LDB	RH3,RL2		; H = LSB
	XORB	RH0,RH0
	RESFLG	C
BNRMLP:
	LDB	RH1,RH0		; Save bit count
	LDB	RH0,RL1		; Get MSB
	ORB	RH0,RH0
	RESFLG	C		; Is it zero?
	JP	NZ,PNORM	; No - Do it bit at a time
	LDB	RL1,RH2		; MSB = NMSB
	LDB	RH2,RH3		; NMSB= LSB
	LDB	RH3,RL3		; LSB = VLSB
	LDB	RL3,RH0		; VLSB= 0
	LDB	RH0,RH1		; Get exponent
	SUBB	RH0,#8		; Count 8 bits
	CPB	RH0,#0E0H	; -24-8 Was number zero?
	JP	NZ,BNRMLP	; No - Keep normalising
RESZER:
	XORB	RH0,RH0
	RESFLG	C		; Result is zero
SAVEXP:
	LDB	(FPEXP),RH0	; Save result as zero
	RET
;
NORMAL:
	DECB	RH1,#1		; Count bits
	ADD	R3,R3		; Shift HL left
	LDB	RH0,RH2		; Get NMSB
	RLCB	RH0,#1		; Shift left with last bit
	LDB	RH2,RH0		; Save NMSB
	LDB	RH0,RL1		; Get MSB
	ADCB	RH0,RH0		; Shift left with last bit
	LDB	RL1,RH0		; Save MSB
PNORM:
	JP	PL,NORMAL	; Not done - Keep going
	LDB	RH0,RH1		; Number of bits shifted
	LDB	RL2,RH3		; Save HL in EB
	LDB	RH1,RL3
	ORB	RH0,RH0
	RESFLG	C		; Any shifting done?
	JP	Z,RONDUP	; No - Round it up
	LD	R3,#FPEXP	; Point to exponent
	ADDB	RH0,@R3		; Add shifted bits
	LDB	@R3,RH0		; Re-save exponent
	JP	NC,RESZER	; Underflow - Result is zero
	RET	Z		; Result is zero
RONDUP:
	LDB	RH0,RH1		; Get VLSB of number
RONDB:
	LD	R3,#FPEXP	; Point to exponent
	ORB	RH0,RH0
	RESFLG	C		; Any rounding?
	JR	PL,RONDB1
	CALL	FPROND		; Yes - Round number up
RONDB1:
	LDB	RH1,@R3		; B = Exponent
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	RH0,@R3		; Get sign of result
	ANDB	RH0,#10000000B	; Only bit 7 needed
	XORB	RH0,RL1
	RESFLG	C		; Set correct sign
	LDB	RL1,RH0		; Save correct sign in number
	JP	FPBCDE		; Move BCDE to FPREG
;
FPROND:
	INCB	RL2,#1		; Round LSB
	RET	NZ		; Return if ok
	INCB	RH2,#1		; Round NMSB
	RET	NZ		; Return if ok
	INCB	RL1,#1		; Round MSB
	RET	NZ		; Return if ok
	LDB	RL1,#80H		; Set normal value
	INCB	@R3,#1		; Increment exponent
	RET	NZ		; Return if ok
	JP	OVERR		; Overflow error
;
PLUCDE:
	LDB	RH0,@R3		; Get LSB of FPREG
	ADDB	RH0,RL2		; Add LSB of BCDE
	LDB	RL2,RH0		; Save LSB of BCDE
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	RH0,@R3		; Get NMSB of FPREG
	ADCB	RH0,RH2		; Add NMSB of BCDE
	LDB	RH2,RH0		; Save NMSB of BCDE
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	RH0,@R3		; Get MSB of FPREG
	ADCB	RH0,RL1		; Add MSB of BCDE
	LDB	RL1,RH0		; Save MSB of BCDE
	RET
;
COMPL:
	LD	R3,#SGNRES	; Sign of result
	LDB	RH0,@R3		; Get sign of result
	LDCTLB	RL4,FLAGS
	COMB	RH0			; Negate it
	LDCTLB	FLAGS,RL4
	LDB	@R3,RH0		; Put it back
	XORB	RH0,RH0
	RESFLG	C
	LDB	RL3,RH0		; Set L to zero
	SUBB	RH0,RH1		; Negate exponent,set carry
	LDB	RH1,RH0		; Re-save exponent
	LDB	RH0,RL3		; Load zero
	SBCB	RH0,RL2		; Negate LSB
	LDB	RL2,RH0		; Re-save LSB
	LDB	RH0,RL3		; Load zero
	SBCB	RH0,RH2		; Negate NMSB
	LDB	RH2,RH0		; Re-save NMSB
	LDB	RH0,RL3		; Load zero
	SBCB	RH0,RL1		; Negate MSB
	LDB	RL1,RH0		; Re-save MSB
	RET
;
SCALE:
	LDB	RH1,#0		; Clear underflow
SCALLP:
	SUBB	RH0,#8		; 8 bits (a whole byte)?
	JP	C,SHRITE	; No - Shift right A bits
	LDB	RH1,RL2		; <- Shift
	LDB	RL2,RH2		; <- right
	LDB	RH2,RL1		; <- eight
	LDB	RL1,#0		; <- bits
	JP	SCALLP		; More bits to shift
;
SHRITE:
	ADDB	RH0,#8+1	; Adjust count
	LDB	RL3,RH0		; Save bits to shift
SHRLP:
	XORB	RH0,RH0
	RESFLG	C		; Flag for all done
	DECB	RL3,#1		; All shifting done?
	RET	Z		; Yes - Return
	LDB	RH0,RL1		; Get MSB
SHRT1:
	RRCB	RH0,#1		; Shift it right
	LDB	RL1,RH0		; Re-save
	LDB	RH0,RH2		; Get NMSB
	RRCB	RH0,#1		; Shift right with last bit
	LDB	RH2,RH0		; Re-save it
	LDB	RH0,RL2		; Get LSB
	RRCB	RH0,#1		; Shift right with last bit
	LDB	RL2,RH0		; Re-save it
	LDB	RH0,RH1		; Get underflow
	RRCB	RH0,#1		; Shift right with last bit
	LDB	RH1,RH0		; Re-save underflow
	JP	SHRLP		; More bits to do
;
UNITY:
	DB	 000H,000H,000H,081H	; 1.00000
;
LOGTAB:
	DB	3			; Table used by LOG
	DB	0AAH,056H,019H,080H	; 0.59898
	DB	0F1H,022H,076H,080H	; 0.96147
	DB	045H,0AAH,038H,082H	; 2.88539
;
	ALIGN	2
LOG:
	CALL	TSTSGN		; Test sign of value
	ORB	RH0,RH0
	RESFLG	C
	JP	PE,FCERR	; ?FC Error if <= zero
	LD	R3,#FPEXP	; Point to exponent
	LDB	RH0,@R3		; Get exponent
	LD	R1,#8035H	; BCDE = SQR(1/2)
	LD	R2,#04F3H
	SUBB	RH0,RH1		; Scale value to be < 1
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Save scale factor
	LDB	@R3,RH1		; Save new exponent
	PUSH	@R15,R2		; Save SQR(1/2)
	PUSH	@R15,R1
	CALL	FPADD		; Add SQR(1/2) to value
	POP	R1,@R15		; Restore SQR(1/2)
	POP	R2,@R15
	INCB	RH1,#1		; Make it SQR(2)
	CALL	DVBCDE		; Divide by SQR(2)
	LD	R3,#UNITY	; Point to 1.
	CALL	SUBPHL		; Subtract FPREG from 1
	LD	R3,#LOGTAB	; Coefficient table
	CALL	SUMSER		; Evaluate sum of series
	LD	R1,#8080H	; BCDE = -0.5
	LD	R2,#0000H
	CALL	FPADD		; Subtract 0.5 from FPREG
	POP	R0,@R15		; Restore scale factor
	LDCTLB	FLAGS,RL0
	CALL	RSCALE		; Re-scale number
MULLN2:
	LD	R1,#8031H	; BCDE = Ln(2)
	LD	R2,#7218H
	JR	FPMULT		; Skip "POP BC" and "POP DE"
;
MULT:
	POP	R1,@R15		; Get number from stack
	POP	R2,@R15
FPMULT:
	CALL	TSTSGN		; Test sign of FPREG
	RET	Z		; Return zero if zero
	LDB	RL3,#0		; Flag add exponents
	CALL	ADDEXP		; Add exponents
	LDB	RH0,RL1		; Get MSB of multiplier
	LDB	(MULVAL),RH0	; Save MSB of multiplier
	EX	R3,R2
	LD	(MULVAL+1),R3	; Save rest of multiplier
	LD	R1,#0		; Partial product (BCDE) = zero
	LDB	RH2,RH1
	LDB	RL2,RH1
	LD	R3,#BNORM	; Address of normalise
	PUSH	@R15,R3		; Save for return
	LD	R3,#MULT8	; Address of 8 bit multiply
	PUSH	@R15,R3		; Save for NMSB,MSB
	PUSH	@R15,R3		;
	LD	R3,#FPREG	; Point to number
MULT8:
	LDB	RH0,@R3		; Get LSB of number
	LDCTLB	RL4,FLAGS
	INC	R3		; Point to NMSB
	LDCTLB	FLAGS,RL4
	ORB	RH0,RH0
	RESFLG	C		; Test LSB
	JP	Z,BYTSFT	; Zero - shift to next byte
	PUSH	@R15,R3		; Save address of number
	LDB	RL3,#8		; 8 bits to multiply by
MUL8LP:
	RRCB	RH0,#1		; Shift LSB right
	LDB	RH3,RH0		; Save LSB
	LDB	RH0,RL1		; Get MSB
	JP	NC,NOMADD	; Bit was zero - Don't add
	PUSH	@R15,R3		; Save LSB and count
	LD	R3,(MULVAL+1)	; Get LSB and NMSB
	ADD	R3,R2		; Add NMSB and LSB
	EX	R3,R2		; Leave sum in DE
	POP	R3,@R15		; Restore MSB and count
	LDB	RH0,(MULVAL)	; Get MSB of multiplier
	ADCB	RH0,RL1		; Add MSB
NOMADD:
	RRCB	RH0,#1		; Shift MSB right
	LDB	RL1,RH0		; Re-save MSB
	LDB	RH0,RH2		; Get NMSB
	RRCB	RH0,#1		; Shift NMSB right
	LDB	RH2,RH0		; Re-save NMSB
	LDB	RH0,RL2		; Get LSB
	RRCB	RH0,#1		; Shift LSB right
	LDB	RL2,RH0		; Re-save LSB
	LDB	RH0,RH1		; Get VLSB
	RRCB	RH0,#1		; Shift VLSB right
	LDB	RH1,RH0		; Re-save VLSB
	DECB	RL3,#1		; Count bits multiplied
	LDB	RH0,RH3		; Get LSB of multiplier
	JP	NZ,MUL8LP	; More - Do it
POPHRT:
	POP	R3,@R15		; Restore address of number
	RET
;
BYTSFT:
	LDB	RH1,RL2		; Shift partial product left
	LDB	RL2,RH2
	LDB	RH2,RL1
	LDB	RL1,RH0
	RET
;
DIV10:
	CALL	STAKFP		; Save FPREG on stack
	LD	R1,#8420H	; BCDE = 10.
	LD	R2,#0000H
	CALL	FPBCDE		; Move 10 to FPREG
;
DIV:
	POP	R1,@R15		; Get number from stack
	POP	R2,@R15
DVBCDE:
	CALL	TSTSGN		; Test sign of FPREG
	JP	Z,DZERR		; Error if division by zero
	LDB	RL3,#-1		; Flag subtract exponents
	CALL	ADDEXP		; Subtract exponents
	INCB	@R3,#1		; Add 2 to exponent to adjust
	INCB	@R3,#1
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; Point to MSB
	LDCTLB	FLAGS,RL4
	LDB	RH0,@R3		; Get MSB of dividend
	LDB	(DIV3),RH0	; Save for subtraction
	LDCTLB	RL4,FLAGS
	DEC	R3,#1
	LDCTLB	FLAGS,RL4
	LDB	RH0,@R3		; Get NMSB of dividend
	LDB	(DIV2),RH0	; Save for subtraction
	LDCTLB	RL4,FLAGS
	DEC	R3,#1
	LDCTLB	FLAGS,RL4
	LDB	RH0,@R3		; Get MSB of dividend
	LDB	(DIV1),RH0	; Save for subtraction
	LDB	RH1,RL1		; Get MSB
	EX	R3,R2		; NMSB,LSB to HL
	XORB	RH0,RH0
	RESFLG	C
	LDB	RL1,RH0		; Clear MSB of quotient
	LDB	RH2,RH0		; Clear NMSB of quotient
	LDB	RL2,RH0		; Clear LSB of quotient
	LDB	(DIV4),RH0	; Clear overflow count
DIVLP:
	PUSH	@R15,R3		; Save divisor
	PUSH	@R15,R1
	LDB	RH0,RL3		; Get LSB of number
	CALL	DIVSUP		; Subt' divisor from dividend
	LDB	RL4,#0
	SBCB	RH0,RL4		; Count for overflows
	COMFLG	C
	JP	NC,RESDIV	; Restore divisor if borrow
	LDB	(DIV4),RH0	; Re-save overflow count
	POP	R0,@R15		; Scrap divisor
	LDCTLB	FLAGS,RL0
	POP	R0,@R15
	LDCTLB	FLAGS,RL0
	SETFLG	C		; Set carry to
	JR	RESDIV1		; Skip "POP BC" and "POP HL"
;
RESDIV:
	POP	R1,@R15		; Restore divisor
	POP	R3,@R15
RESDIV1:
	LDB	RH0,RL1		; Get MSB of quotient
	INCB	RH0,#1
	DECB	RH0,#1
	LDCTLB	RL4,FLAGS
	RRCB	RH0,#1		; Bit 0 to bit 7
	LDCTLB	FLAGS,RL4
	JP	MI,RONDB	; Done - Normalise result
	RLCB	RH0,#1		; Restore carry
	LDB	RH0,RL2		; Get LSB of quotient
	RLCB	RH0,#1		; Double it
	LDB	RL2,RH0		; Put it back
	LDB	RH0,RH2		; Get NMSB of quotient
	RLCB	RH0,#1		; Double it
	LDB	RH2,RH0		; Put it back
	LDB	RH0,RL1		; Get MSB of quotient
	RLCB	RH0,#1		; Double it
	LDB	RL1,RH0		; Put it back
	ADD	R3,R3		; Double NMSB,LSB of divisor
	LDB	RH0,RH1		; Get MSB of divisor
	RLCB	RH0,#1		; Double it
	LDB	RH1,RH0		; Put it back
	LDB	RH0,(DIV4)	; Get VLSB of quotient
	RLCB	RH0,#1		; Double it
	LDB	(DIV4),RH0	; Put it back
	LDB	RH0,RL1		; Get MSB of quotient
	ORB	RH0,RH2		; Merge NMSB
	RESFLG	C
	ORB	RH0,RL2		; Merge LSB
	RESFLG	C
	JP	NZ,DIVLP	; Not done - Keep dividing
	PUSH	@R15,R3		; Save divisor
	LD	R3,#FPEXP	; Point to exponent
	DECB	@R3		; Divide by 2
	POP	R3,@R15		; Restore divisor
	JP	NZ,DIVLP	; Ok - Keep going
	JP	OVERR		; Overflow error
;
ADDEXP:
	LDB	RH0,RH1		; Get exponent of dividend
	ORB	RH0,RH0
	RESFLG	C		; Test it
	JP	Z,OVTST3	; Zero - Result zero
	LDB	RH0,RL3		; Get add/subtract flag
	LD	R3,#FPEXP	; Point to exponent
	XORB	RH0,@R3
;	RESFLG	C		; Add or subtract it
	ADDB	RH0,RH1		; Add the other exponent
	LDB	RH1,RH0		; Save new exponent
	RRCB	RH0,#1		; Test exponent for overflow
	XORB	RH0,RH1
;	RESFLG	C
	LDB	RH0,RH1		; Get exponent
	JP	PL,OVTST2	; Positive - Test for overflow
	ADDB	RH0,#80H	; Add excess 128
	LDB	@R3,RH0		; Save new exponent
	JP	Z,POPHRT	; Zero - Result zero
	CALL	SIGNS		; Set MSBs and sign of result
	LDB	@R3,RH0		; Save new exponent
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; Point to MSB
	LDCTLB	FLAGS,RL4
	RET
;
OVTST1:
	CALL	TSTSGN		; Test sign of FPREG
	LDCTLB	RL4,FLAGS
	COMB	RH0		; Invert sign
	LDCTLB	FLAGS,RL4
	POP	R3,@R15		; Clean up stack
OVTST2:
	ORB	RH0,RH0
	RESFLG	C		; Test if new exponent zero
OVTST3:
	POP	R3,@R15		; Clear off return address
	JP	PL,RESZER	; Result zero
	JP	OVERR		; Overflow error
;
MLSP10:
	CALL	BCDEFP		; Move FPREG to BCDE
	LDB	RH0,RH1		; Get exponent
	ORB	RH0,RH0
	RESFLG	C		; Is it zero?
	RET	Z		; Yes - Result is zero
	ADDB	RH0,#2		; Multiply by 4
	JP	C,OVERR		; Overflow - ?OV Error
	LDB	RH1,RH0		; Re-save exponent
	CALL	FPADD		; Add BCDE to FPREG (Times 5)
	LD	R3,#FPEXP	; Point to exponent
	INCB	@R3,#1		; Double number (Times 10)
	RET	NZ		; Ok - Return
	JP	OVERR		; Overflow error
;
TSTSGN:
	LDB	RH0,(FPEXP)	; Get sign of FPREG
	ORB	RH0,RH0
	RESFLG	C
	RET	Z		; RETurn if number is zero
	LDB	RH0,(FPREG+2)	; Get MSB of FPREG
	CPB	RH0,#02FH	; Test sign
	JR	RETREL1
RETREL:
	LDCTLB	RL4,FLAGS
	COMB	RH0		; Invert sign
	LDCTLB	FLAGS,RL4
RETREL1:
	RLCB	RH0,#1		; Sign bit to carry
FLGDIF:
	SBCB	RH0,RH0		; Carry to all bits of A
	RET	NZ		; Return -1 if negative
	INCB	RH0,#1		; Bump to +1
	RET			; Positive - Return +1
;
SGN:
	CALL	TSTSGN		; Test sign of FPREG
FLGREL:
	LDB	RH1,#80H+8	; 8 bit integer in exponent
	LD	R2,#0		; Zero NMSB and LSB
RETINT:
	LD	R3,#FPEXP	; Point to exponent
	LDB	RL1,RH0		; CDE = MSB,NMSB and LSB
	LDB	@R3,RH1		; Save exponent
	LDB	RH1,#0		; CDE = integer to normalise
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4	; Point to sign of result
	LDB	@R3,#80H	; Set sign of result
	RLCB	RH0,#1		; Carry = sign of integer
	JP	CONPOS		; Set sign of result
;
ABS:
	CALL	TSTSGN		; Test sign of FPREG
	RET	PL		; Return if positive
INVSGN:
	LD	R3,#FPREG+2	; Point to MSB
	LDB	RH0,@R3		; Get sign of mantissa
	XORB	RH0,#80H	; Invert sign of mantissa
	RESFLG	C
	LDB	@R3,RH0		; Re-save sign of mantissa
	RET
;
STAKFP:
	EX	R3,R2		; Save code string address
	LD	R3,(FPREG)	; LSB,NLSB of FPREG
	EXB	RH3,RL3
	EX	@R15,R3		; Stack them,get return
	PUSH	@R15,R3		; Re-save return
	LD	R3,(FPREG+2)	; MSB and exponent of FPREG
	EXB	RH3,RL3
	EX	@R15,R3		; Stack them,get return
	PUSH	@R15,R3		; Re-save return
	EX	R3,R2		; Restore code string address
	RET
;
PHLTFP:
	CALL	LOADFP		; Number at HL to BCDE
FPBCDE:
	EX	R3,R2		; Save code string address
	EXB	RH3,RL3
	LD	(FPREG),R3	; Save LSB,NLSB of number
	EXB	RH3,RL3
	LDB	RH3,RH1		; Exponent of number
	LDB	RL3,RL1		; MSB of number
	EXB	RH3,RL3
	LD	(FPREG+2),R3	; Save MSB and exponent
	EXB	RH3,RL3
	EX	R3,R2		; Restore code string address
	RET
;
BCDEFP:
	LD	R3,#FPREG	; Point to FPREG
LOADFP:
	LDB	RL2,@R3		; Get LSB of number
	LDCTLB	RL4,FLAGS
	INC	R3
	LDB	RH2,@R3		; Get NMSB of number
	INC	R3
	LDB	RL1,@R3		; Get MSB of number
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	RH1,@R3		; Get exponent of number
INCHL:
	LDCTLB	RL4,FLAGS
	INC	R3		; Used for conditional "INC HL"
	LDCTLB	FLAGS,RL4
	RET
;
FPTHL:
	LD	R2,#FPREG	; Point to FPREG
DETHL4:
	LDB	RH1,#4		; 4 bytes to move
DETHLB:
	LDB	RH0,@R2		; Get source
	LDB	@R3,RH0		; Save destination
	LDCTLB	RL4,FLAGS
	INC	R2		; Next source
	INC	R3		; Next destination
	LDCTLB	FLAGS,RL4
	DECB	RH1,#1		; Count bytes
	JP	NZ,DETHLB	; Loop if more
	RET
;
SIGNS:
	LD	R3,#FPREG+2	; Point to MSB of FPREG
	LDB	RH0,@R3		; Get MSB
	RLB	RH0,#1		; Old sign to carry
	SETFLG	C		; Set MSBit
	RRCB	RH0,#1		; Set MSBit of MSB
	LDB	@R3,RH0		; Save new MSB
	COMFLG	C		; Complement sign
	RRCB	RH0,#1		; Old sign to carry
	LDCTLB	RL4,FLAGS
	INC	R3
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	@R3,RH0		; Set sign of result
	LDB	RH0,RL1		; Get MSB
	RLB	RH0,#1		; Old sign to carry
	SETFLG	C		; Set MSBit
	RRCB	RH0,#1		; Set MSBit of MSB
	LDB	RL1,RH0		; Save MSB
	RRCB	RH0,#1
	XORB	RH0,@R3
	RESFLG	C		; New sign of result
	RET
;
CMPNUM:
	LDB	RH0,RH1		; Get exponent of number
	ORB	RH0,RH0
	RESFLG	C
	JP	Z,TSTSGN	; Zero - Test sign of FPREG
	LD	R3,#RETREL	; Return relation routine
	PUSH	@R15,R3		; Save for return
	CALL	TSTSGN		; Test sign of FPREG
	LDB	RH0,RL1		; Get MSB of number
	RET	Z		; FPREG zero - Number's MSB
	LD	R3,#FPREG+2	; MSB of FPREG
	XORB	RH0,@R3
	RESFLG	C		; Combine signs
	LDB	RH0,RL1		; Get MSB of number
	RET	MI		; Exit if signs different
	CALL	CMPFP		; Compare FP numbers
	RRCB	RH0,#1		; Get carry to sign
	XORB	RH0,RL1
	RESFLG	C		; Combine with MSB of number
	RET
;
CMPFP:
	LDCTLB	RL4,FLAGS
	INC	R3		; Point to exponent
	LDCTLB	FLAGS,RL4
	LDB	RH0,RH1		; Get exponent
	CPB	RH0,@R3		; Compare exponents
	RET	NZ		; Different
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; Point to MBS
	LDCTLB	FLAGS,RL4
	LDB	RH0,RL1		; Get MSB
	CPB	RH0,@R3		; Compare MSBs
	RET	NZ		; Different
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; Point to NMSB
	LDCTLB	FLAGS,RL4
	LDB	RH0,RH2		; Get NMSB
	CPB	RH0,@R3		; Compare NMSBs
	RET	NZ		; Different
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; Point to LSB
	LDCTLB	FLAGS,RL4
	LDB	RH0,RL2		; Get LSB
	SUBB	RH0,@R3		; Compare LSBs
	RET	NZ		; Different
	POP	R3,@R15		; Drop RETurn
	POP	R3,@R15		; Drop another RETurn
	RET
;
FPINT:
	LDB	RH1,RH0		; <- Move
	LDB	RL1,RH0		; <- exponent
	LDB	RH2,RH0		; <- to all
	LDB	RL2,RH0		; <- bits
	ORB	RH0,RH0
	RESFLG	C		; Test exponent
	RET	Z		; Zero - Return zero
	PUSH	@R15,R3		; Save pointer to number
	CALL	BCDEFP		; Move FPREG to BCDE
	CALL	SIGNS		; Set MSBs & sign of result
	XORB	RH0,@R3
	RESFLG	C		; Combine with sign of FPREG
	LDB	RH3,RH0		; Save combined signs
	JR	PL,FPINT1
	CALL	DCBCDE		; Negative - Decrement BCDE
FPINT1:
	LDB	RH0,#80H+24	; 24 bits
	SUBB	RH0,RH1		; Bits to shift
	CALL	SCALE		; Shift BCDE
	LDB	RH0,RH3		; Get combined sign
	RLCB	RH0,#1		; Sign to carry
	JR	NC,FPINT2
	CALL	FPROND		; Negative - Round number up
FPINT2:
	LDB	RH1,#0		; Zero exponent
	JR	NC,FPINT3
	CALL	COMPL		; If negative make positive
FPINT3:
	POP	R3,@R15		; Restore pointer to number
	RET
;
DCBCDE:
	LDCTLB	RL4,FLAGS
	DEC	R2,#1		; Decrement BCDE
	LDCTLB	FLAGS,RL4
	LDB	RH0,RH2		; Test LSBs
	ANDB	RH0,RL2
	RESFLG	C
	INCB	RH0,#1
	RET	NZ		; Exit if LSBs not FFFF
	LDCTLB	RL4,FLAGS
	DEC	R1,#1		; Decrement MSBs
	LDCTLB	FLAGS,RL4
	RET
;
INT:
	LD	R3,#FPEXP	; Point to exponent
	LDB	RH0,@R3		; Get exponent
	CPB	RH0,#80H+24	; Integer accuracy only?
	LDB	RH0,(FPREG)	; Get LSB
	RET	NC		; Yes - Already integer
	LDB	RH0,@R3		; Get exponent
	CALL	FPINT		; F.P to integer
	LDB	@R3,#80H+24	; Save 24 bit integer
	LDB	RH0,RL2		; Get LSB of number
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Save LSB
	LDB	RH0,RL1		; Get MSB of number
	RLCB	RH0		; Sign to carry
	CALL	CONPOS		; Set sign of result
	POP	R0,@R15		; Restore LSB of number
	LDCTLB	FLAGS,RL0
	RET
;
MLDEBC:
	LD	R3,#0		; Clear partial product
	LDB	RH0,RH1		; Test multiplier
	ORB	RH0,RL1
	RESFLG	C
	RET	Z		; Return zero if zero
	LDB	RH0,#16		; 16 bits
MLDBLP:
	ADD	R3,R3		; Shift P.P left
	JP	C,BSERR		; ?BS Error if overflow
	EX	R3,R2
	ADD	R3,R3		; Shift multiplier left
	EX	R3,R2
	JP	NC,NOMLAD	; Bit was zero - No add
	ADD	R3,R1		; Add multiplicand
	JP	C,BSERR		; ?BS Error if overflow
NOMLAD:
	DECB	RH0,#1		; Count bits
	JP	NZ,MLDBLP	; More
	RET
;
ASCTFP:
	CPB	RH0,#'-'	; Negative?
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Save it and flags
	JP	Z,CNVNUM	; Yes - Convert number
	CPB	RH0,#'+'	; Positive?
	JP	Z,CNVNUM	; Yes - Convert number
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; DEC 'cos GETCHR INCs
	LDCTLB	FLAGS,RL4
CNVNUM:
	CALL	RESZER		; Set result to zero
	LDB	RH1,RH0		; Digits after point counter
	LDB	RH2,RH0		; Sign of exponent
	LDB	RL2,RH0		; Exponent of ten
	LDCTLB	RL4,FLAGS
	COMB	RH0
	LDCTLB	FLAGS,RL4
	LDB	RL1,RH0		; Before or after point flag
MANLP:
	CALL	GETCHR		; Get next character
	JP	C,ADDIG		; Digit - Add to number
	CPB	RH0,#'.'
	JP	Z,DPOINT	; '.' - Flag point
	CPB	RH0,#'E'
	JP	NZ,CONEXP	; Not 'E' - Scale number
	CALL	GETCHR		; Get next character
	CALL	SGNEXP		; Get sign of exponent
EXPLP:
	CALL	GETCHR		; Get next character
	JP	C,EDIGIT	; Digit - Add to exponent
	INCB	RH2,#1		; Is sign negative?
	JP	NZ,CONEXP	; No - Scale number
	XORB	RH0,RH0
	RESFLG	C
	SUBB	RH0,RL2		; Negate exponent
	LDB	RL2,RH0		; And re-save it
	INCB	RL1,#1		; Flag end of number
DPOINT:
	INCB	RL1,#1		; Flag point passed
	JP	Z,MANLP		; Zero - Get another digit
CONEXP:
	PUSH	@R15,R3		; Save code string address
	LDB	RH0,RL2		; Get exponent
	SUBB	RH0,RH1		; Subtract digits after point
SCALMI:
	JR	MI,SCALMI1
	CALL	SCALPL		; Positive - Multiply number
SCALMI1:
	JP	PL,ENDCON	; Positive - All done
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Save number of times to /10
	CALL	DIV10		; Divide by 10
	POP	R0,@R15		; Restore count
	LDCTLB	FLAGS,RL0
	INCB	RH0,#1		; Count divides
;
ENDCON:
	JP	NZ,SCALMI	; More to do
	POP	R2,@R15		; Restore code string address
	POP	R0,@R15		; Restore sign of number
	LDCTLB	FLAGS,RL0
	JR	NZ,ENDCON1
	CALL	INVSGN		; Negative - Negate number
ENDCON1:
	EX	R3,R2		; Code string address to HL
	RET
;
SCALPL:
	RET	Z		; Exit if no scaling needed
MULTEN:
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Save count
	CALL	MLSP10		; Multiply number by 10
	POP	R0,@R15		; Restore count
	LDCTLB	FLAGS,RL0
	DECB	RH0,#1		; Count multiplies
	RET
;
ADDIG:
	PUSH	@R15,R2		; Save sign of exponent
	LDB	RH2,RH0		; Save digit
	LDB	RH0,RH1		; Get digits after point
	ADCB	RH0,RL1		; Add one if after point
	LDB	RH1,RH0		; Re-save counter
	PUSH	@R15,R1		; Save point flags
	PUSH	@R15,R3		; Save code string address
	PUSH	@R15,R2		; Save digit
	CALL	MLSP10		; Multiply number by 10
	POP	R0,@R15		; Restore digit
	LDCTLB	FLAGS,RL0
	SUBB	RH0,#'0'	; Make it absolute
	CALL	RSCALE		; Re-scale number
	POP	R3,@R15		; Restore code string address
	POP	R1,@R15		; Restore point flags
	POP	R2,@R15		; Restore sign of exponent
	JP	MANLP		; Get another digit
;
RSCALE:
	CALL	STAKFP		; Put number on stack
	CALL	FLGREL		; Digit to add to FPREG
PADD:
	POP	R1,@R15		; Restore number
	POP	R2,@R15
	JP	FPADD		; Add BCDE to FPREG and return
;
EDIGIT:
	LDB	RH0,RL2		; Get digit
	RLB	RH0		; Times 2
	RLB	RH0		; Times 4
	ADDB	RH0,RL2		; Times 5
	RLB	RH0		; Times 10
	ADDB	RH0,@R3		; Add next digit
	SUBB	RH0,#'0'	; Make it absolute
	LDB	RL2,RH0		; Save new digit
	JP	EXPLP		; Look for another digit
;
LINEIN:
	PUSH	@R15,R3		; Save code string address
	LD	R3,#INMSG	; Output " in "
	CALL	PRS		; Output string at HL
	POP	R3,@R15		; Restore code string address
PRNTHL:
	EX	R3,R2		; Code string address to DE
	XORB	RH0,RH0
	RESFLG	C
	LDB	RH1,#80H+24	; 24 bits
	CALL	RETINT		; Return the integer
	LD	R3,#PRNUMS	; Print number string
	PUSH	@R15,R3		; Save for return
NUMASC:
	LD	R3,#PBUFF	; Convert number to ASCII
	PUSH	@R15,R3		; Save for return
	CALL	TSTSGN		; Test sign of FPREG
	LDB	@R3,#' '	; Space at start
	JP	PL,SPCFST	; Positive - Space to start
	LDB	@R3,#'-'	; '-' sign at start
SPCFST:
	LDCTLB	RL4,FLAGS
	INC	R3		; First byte of number
	LDCTLB	FLAGS,RL4
	LDB	@R3,#'0'	; '0' if zero
	JP	Z,JSTZER	; Return '0' if zero
	PUSH	@R15,R3		; Save buffer address
	JR	PL,SPCFST1
	CALL	INVSGN		; Negate FPREG if negative
SPCFST1:
	XORB	RH0,RH0
	RESFLG	C		; Zero A
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Save it
	CALL	RNGTST		; Test number is in range
SIXDIG:
	LD	R1,#9143H	; BCDE - 99999.9
	LD	R2,#4FF8H
	CALL	CMPNUM		; Compare numbers
	ORB	RH0,RH0
	RESFLG	C
	JP	PO,INRNG	; > 99999.9 - Sort it out
	POP	R0,@R15		; Restore count
	LDCTLB	FLAGS,RL0
	CALL	MULTEN		; Multiply by ten
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Re-save count
	JP	SIXDIG		; Test it again
;
GTSIXD:
	CALL	DIV10		; Divide by 10
	POP	R0,@R15		; Get count
	LDCTLB	FLAGS,RL0
	INCB	RH0,#1		; Count divides
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Re-save count
	CALL	RNGTST		; Test number is in range
INRNG:
	CALL	ROUND		; Add 0.5 to FPREG
	INCB	RH0,#1
	CALL	FPINT		; F.P to integer
	CALL	FPBCDE		; Move BCDE to FPREG
	LD	R1,#0306H	; 1E+06 to 1E-03 range
	POP	R0,@R15		; Restore count
	LDCTLB	FLAGS,RL0
	ADDB	RH0,RL1		; 6 digits before point
	INCB	RH0,#1		; Add one
	JP	MI,MAKNUM	; Do it in 'E' form if < 1E-02
	CPB	RH0,#6+1+1	; More than 999999 ?
	JP	NC,MAKNUM	; Yes - Do it in 'E' form
	INCB	RH0,#1		; Adjust for exponent
	LDB	RH1,RH0		; Exponent of number
	LDB	RH0,#2		; Make it zero after
;
MAKNUM:
	DECB	RH0,#1		; Adjust for digits to do
	DECB	RH0,#1
	POP	R3,@R15		; Restore buffer address
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Save count
	LD	R2,#POWERS	; Powers of ten
	SUBB	RH1,#1		; Count digits before point
	JP	NZ,DIGTXT	; Not zero - Do number
	LDB	@R3,#'.'	; Save point
	LDCTLB	RL4,FLAGS
	INC	R3		; Move on
	LDCTLB	FLAGS,RL4
	LDB	@R3,#'0'	; Save zero
	LDCTLB	RL4,FLAGS
	INC	R3		; Move on
	LDCTLB	FLAGS,RL4
DIGTXT:
	DECB	RH1,#1		; Count digits before point
	LDB	@R3,#'.'	; Save point in case
	JR	NZ,DIGTXT1
	CALL	INCHL		; Last digit - move on
DIGTXT1:
	PUSH	@R15,R1		; Save digits before point
	PUSH	@R15,R3		; Save buffer address
	PUSH	@R15,R2		; Save powers of ten
	CALL	BCDEFP		; Move FPREG to BCDE
	POP	R3,@R15		; Powers of ten table
	LDB	RH1,#'0'-1	; ASCII '0' - 1
TRYAGN:
	INCB	RH1,#1		; Count subtractions
	LDB	RH0,RL2		; Get LSB
	SUBB	RH0,@R3		; Subtract LSB
	LDB	RL2,RH0		; Save LSB
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	RH0,RH2		; Get NMSB
	LDB	RL4,@R3
	SBCB	RH0,RL4		; Subtract NMSB
	LDB	RH2,RH0		; Save NMSB
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	RH0,RL1		; Get MSB
	LDB	RL4,@R3
	SBCB	RH0,RL4		; Subtract MSB
	LDB	RL1,RH0		; Save MSB
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; Point back to start
	LDCTLB	FLAGS,RL4
	LDCTLB	RL4,FLAGS
	DEC	R3,#1
	LDCTLB	FLAGS,RL4
	JP	NC,TRYAGN	; No overflow - Try again
	CALL	PLUCDE		; Restore number
	LDCTLB	RL4,FLAGS
	INC	R3		; Start of next number
	LDCTLB	FLAGS,RL4
	CALL	FPBCDE		; Move BCDE to FPREG
	EX	R3,R2		; Save point in table
	POP	R3,@R15		; Restore buffer address
	LDB	@R3,RH1		; Save digit in buffer
	LDCTLB	RL4,FLAGS
	INC	R3		; And move on
	LDCTLB	FLAGS,RL4
	POP	R1,@R15		; Restore digit count
	DECB	RL1,#1		; Count digits
	JP	NZ,DIGTXT	; More - Do them
	DECB	RH1,#1		; Any decimal part?
	JP	Z,DOEBIT	; No - Do 'E' bit
SUPTLZ:
	LDCTLB	RL4,FLAGS
	DEC	R3,#1
	LDCTLB	FLAGS,RL4	; Move back through buffer
	LDB	RH0,@R3		; Get character
	CPB	RH0,#'0'	; '0' character?
	JP	Z,SUPTLZ	; Yes - Look back for more
	CPB	RH0,#'.'	; A decimal point?
	JR	Z,DOEBIT
	CALL	INCHL		; Move back over digit
;
DOEBIT:
	POP	R0,@R15		; Get 'E' flag
	LDCTLB	FLAGS,RL0
	JP	Z,NOENED	; No 'E' needed - End buffer
	LDB	@R3,#'E'	; Put 'E' in buffer
	LDCTLB	RL4,FLAGS
	INC	R3		; And move on
	LDCTLB	FLAGS,RL4
	LDB	@R3,#'+'	; Put '+' in buffer
	JP	PL,OUTEXP	; Positive - Output exponent
	LDB	@R3,#'-'	; Put '-' in buffer
	LDCTLB	RL4,FLAGS
	COMB	RH0		; Negate exponent
	LDCTLB	FLAGS,RL4
	INCB	RH0,#1
OUTEXP:
	LDB	RH1,#'0'-1	; ASCII '0' - 1
EXPTEN:
	INCB	RH1,#1		; Count subtractions
	SUBB	RH0,#10		; Tens digit
	JP	NC,EXPTEN	; More to do
	ADDB	RH0,#'0'+10	; Restore and make ASCII
	LDCTLB	RL4,FLAGS
	INC	R3		; Move on
	LDCTLB	FLAGS,RL4
	LDB	@R3,RH1		; Save MSB of exponent
JSTZER:
	LDCTLB	RL4,FLAGS
	INC	R3		;
	LDCTLB	FLAGS,RL4
	LDB	@R3,RH0		; Save LSB of exponent
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
NOENED:
	LDB	@R3,RL1		; Mark end of buffer
	POP	R3,@R15		; Restore code string address
	RET
;
RNGTST:
	LD	R1,#9474H	; BCDE = 999999.
	LD	R2,#23F7H
	CALL	CMPNUM		; Compare numbers
	ORB	RH0,RH0
	RESFLG	C
	POP	R3,@R15		; Return address to HL
	JP	PO,GTSIXD	; Too big - Divide by ten
	JP	@R3		; Otherwise return to caller
;
HALF:
	DB	00H,00H,00H,80H	; 0.5
;
	ALIGN	2
POWERS:
	DB	0A0H,086H,001H	; 100000
	DB	010H,027H,000H	; 10000
	DB	0E8H,003H,000H	; 1000
	DB	064H,000H,000H	; 100
	DB	00AH,000H,000H	; 10
	DB	001H,000H,000H	; 1
;
	ALIGN	2
NEGAFT:
	LD	R3,#INVSGN	; Negate result
	EX	@R15,R3		; To be done after caller
	JP	@R3		; Return to caller
;
SQR:
	CALL	STAKFP		; Put value on stack
	LD	R3,#HALF	; Set power to 1/2
	CALL	PHLTFP		; Move 1/2 to FPREG
;
POWER:
	POP	R1,@R15		; Get base
	POP	R2,@R15
	CALL	TSTSGN		; Test sign of power
	LDB	RH0,RH1		; Get exponent of base
	JP	Z,EXP		; Make result 1 if zero
	JP	PL,POWER1	; Positive base - Ok
	ORB	RH0,RH0
	RESFLG	C		; Zero to negative power?
	JP	Z,DZERR		; Yes - ?/0 Error
POWER1:
	ORB	RH0,RH0
	RESFLG	C		; Base zero?
	JP	Z,SAVEXP	; Yes - Return zero
	PUSH	@R15,R2		; Save base
	PUSH	@R15,R1
	LDB	RH0,RL1		; Get MSB of base
	ORB	RH0,#01111111B	; Get sign status
	RESFLG	C
	CALL	BCDEFP		; Move power to BCDE
	JP	PL,POWER2	; Positive base - Ok
	PUSH	@R15,R2		; Save power
	PUSH	@R15,R1
	CALL	INT		; Get integer of power
	POP	R1,@R15		; Restore power
	POP	R2,@R15
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; MSB of base
	CALL	CMPNUM		; Power an integer?
	POP	R3,@R15		; Restore MSB of base
	LDB	RH0,RH3		; but don't affect flags
	RRCB	RH0,#1		; Exponent odd or even?
POWER2:
	POP	R3,@R15		; Restore MSB and exponent
	EXB	RH3,RL3
	LD	(FPREG+2),R3	; Save base in FPREG
	EXB	RH3,RL3
	POP	R3,@R15		; LSBs of base
	EXB	RH3,RL3
	LD	(FPREG),R3	; Save in FPREG
	EXB	RH3,RL3
	JR	NC,POWER21
	CALL	NEGAFT		; Odd power - Negate result
POWER21:
	JR	NZ,POWER22
	CALL	INVSGN		; Negative base - Negate it
POWER22:
	PUSH	@R15,R2		; Save power
	PUSH	@R15,R1
	CALL	LOG		; Get LOG of base
	POP	R1,@R15		; Restore power
	POP	R2,@R15
	CALL	FPMULT		; Multiply LOG by power
;
EXP:
	CALL	STAKFP		; Put value on stack
	LD	R1,#08138H	; BCDE = 1/Ln(2)
	LD	R2,#0AA3BH
	CALL	FPMULT		; Multiply value by 1/LN(2)
	LDB	RH0,(FPEXP)	; Get exponent
	CPB	RH0,#80H+8	; Is it in range?
	JP	NC,OVTST1	; No - Test for overflow
	CALL	INT		; Get INT of FPREG
	ADDB	RH0,#80H	; For excess 128
	ADDB	RH0,#2		; Exponent > 126?
	JP	C,OVTST1	; Yes - Test for overflow
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Save scaling factor
	LD	R3,#UNITY	; Point to 1.
	CALL	ADDPHL		; Add 1 to FPREG
	CALL	MULLN2		; Multiply by LN(2)
	POP	R0,@R15		; Restore scaling factor
	LDCTLB	FLAGS,RL0
	POP	R1,@R15		; Restore exponent
	POP	R2,@R15
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Save scaling factor
	CALL	SUBCDE		; Subtract exponent from FPREG
	CALL	INVSGN		; Negate result
	LD	R3,#EXPTAB	; Coefficient table
	CALL	SMSER1		; Sum the series
	LD	R2,#0		; Zero LSBs
	POP	R1,@R15		; Scaling factor
	LDB	RL1,RH2		; Zero MSB
	JP	FPMULT		; Scale result to correct value
;
EXPTAB:
	DB	8			; Table used by EXP
	DB	040H,02EH,094H,074H	; -1/7! (-1/5040)
	DB	070H,04FH,02EH,077H	;  1/6! ( 1/720)
	DB	06EH,002H,088H,07AH	; -1/5! (-1/120)
	DB	0E6H,0A0H,02AH,07CH	;  1/4! ( 1/24)
	DB	050H,0AAH,0AAH,07EH	; -1/3! (-1/6)
	DB	0FFH,0FFH,07FH,07FH	;  1/2! ( 1/2)
	DB	000H,000H,080H,081H	; -1/1! (-1/1)
	DB	000H,000H,000H,081H	;  1/0! ( 1/1)
;
	ALIGN	2
SUMSER:
	CALL	STAKFP		; Put FPREG on stack
	LD	R2,#MULT	; Multiply by "X"
	PUSH	@R15,R2		; To be done after
	PUSH	@R15,R3		; Save address of table
	CALL	BCDEFP		; Move FPREG to BCDE
	CALL	FPMULT		; Square the value
	POP	R3,@R15		; Restore address of table
SMSER1:
	CALL	STAKFP		; Put value on stack
	LDB	RH0,@R3		; Get number of coefficients
	LDCTLB	RL4,FLAGS
	INC	R3		; Point to start of table
	LDCTLB	FLAGS,RL4
	CALL	PHLTFP		; Move coefficient to FPREG
	JR	SUMLP1		; Skip "POP AF"
SUMLP:
	POP	R0,@R15		; Restore count
	LDCTLB	FLAGS,RL0
SUMLP1:
	POP	R1,@R15		; Restore number
	POP	R2,@R15
	DECB	RH0,#1		; Cont coefficients
	RET	Z		; All done
	PUSH	@R15,R2		; Save number
	PUSH	@R15,R1
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Save count
	PUSH	@R15,R3		; Save address in table
	CALL	FPMULT		; Multiply FPREG by BCDE
	POP	R3,@R15		; Restore address in table
	CALL	LOADFP		; Number at HL to BCDE
	PUSH	@R15,R3		; Save address in table
	CALL	FPADD		; Add coefficient to FPREG
	POP	R3,@R15		; Restore address in table
	JP	SUMLP		; More coefficients
;
RND:
	CALL	TSTSGN		; Test sign of FPREG
	LD	R3,#SEED+2	; Random number seed
	JP	MI,RESEED	; Negative - Re-seed
	LD	R3,#LSTRND	; Last random number
	CALL	PHLTFP		; Move last RND to FPREG
	LD	R3,#SEED+2	; Random number seed
	RET	Z		; Return if RND(0)
	ADDB	RH0,@R3		; Add (SEED)+2)
	ANDB	RH0,#00000111B	; 0 to 7
	LDB	RH1,#0
	LDB	@R3,RH0		; Re-save seed
	LDCTLB	RL4,FLAGS
	INC	R3		; Move to coefficient table
	LDCTLB	FLAGS,RL4
	ADDB	RH0,RH0		; 4 bytes
	ADDB	RH0,RH0		; per entry
	LDB	RL1,RH0		; BC = Offset into table
	ADD	R3,R1		; Point to coefficient
	CALL	LOADFP		; Coefficient to BCDE
	EXB	RH2,RL2
	EXB	RH1,RL1
	CALL	FPMULT		; Multiply FPREG by coefficient
	LDB	RH0,(SEED+1)	; Get (SEED+1)
	INCB	RH0,#1		; Add 1
	ANDB	RH0,#00000011B	; 0 to 3
	LDB	RH1,#0
	CPB	RH0,#1		; Is it zero?
	ADCB	RH0,RH1		; Yes - Make it 1
	LDB	(SEED+1),RH0	; Re-save seed
	LD	R3,#RNDTAB-4	; Addition table
	ADDB	RH0,RH0		; 4 bytes
	ADDB	RH0,RH0		; per entry
	LDB	RL1,RH0		; BC = Offset into table
	ADD	R3,R1		; Point to value
	CALL	ADDPHL		; Add value to FPREG
RND1:
	CALL	BCDEFP		; Move FPREG to BCDE
	LDB	RH0,RL2		; Get LSB
	LDB	RL2,RL1		; LSB = MSB
	XORB	RH0,#01001111B	; Fiddle around
	RESFLG	C
	LDB	RL1,RH0		; New MSB
	LDB	@R3,#80H	; Set exponent
	LDCTLB	RL4,FLAGS
	DEC	R3,#1		; Point to MSB
	LDCTLB	FLAGS,RL4
	LDB	RH1,@R3		; Get MSB
	LDB	@R3,#80H	; Make value -0.5
	LD	R3,#SEED	; Random number seed
	INCB	@R3,#1		; Count seed
	LDB	RH0,@R3		; Get seed
	SUBB	RH0,#171	; Do it modulo 171
	JP	NZ,RND2		; Non-zero - Ok
	LDB	@R3,RH0		; Zero seed
	INCB	RL1,#1		; Fillde about
	DECB	RH2,#1		; with the
	INCB	RL2,#1		; number
RND2:
	CALL	BNORM		; Normalise number
	LD	R3,#LSTRND	; Save random number
	JP	FPTHL		; Move FPREG to last and return
;
RESEED:
	LDB	@R3,RH0		; Re-seed random numbers
	DEC	R3,#1
	LDB	@R3,RH0
	DEC	R3,#1
	LDB	@R3,RH0
	JP	RND1		; Return RND seed
;
RNDTAB:
	DB	068H,0B1H,046H,068H	; Table used by RND
	DB	099H,0E9H,092H,069H
	DB	010H,0D1H,075H,068H
;
	ALIGN	2
COS:
	LD	R3,#HALFPI	; Point to PI/2
	CALL	ADDPHL		; Add it to PPREG
SIN:
	CALL	STAKFP		; Put angle on stack
	LD	R1,#8349H	; BCDE = 2 PI
	LD	R2,#0FDBH
	CALL	FPBCDE		; Move 2 PI to FPREG
	POP	R1,@R15		; Restore angle
	POP	R2,@R15
	CALL	DVBCDE		; Divide angle by 2 PI
	CALL	STAKFP		; Put it on stack
	CALL	INT		; Get INT of result
	POP	R1,@R15		; Restore number
	POP	R2,@R15
	CALL	SUBCDE		; Make it 0 <= value < 1
	LD	R3,#QUARTR	; Point to 0.25
	CALL	SUBPHL		; Subtract value from 0.25
	CALL	TSTSGN		; Test sign of value
	SETFLG	C		; Flag positive
	JP	PL,SIN1		; Positive - Ok
	CALL	ROUND		; Add 0.5 to value
	CALL	TSTSGN		; Test sign of value
	ORB	RH0,RH0
	RESFLG	C		; Flag negative
SIN1:
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Save sign
	JR	MI,SIN11
	CALL	INVSGN		; Negate value if positive
SIN11:
	LD	R3,#QUARTR	; Point to 0.25
	CALL	ADDPHL		; Add 0.25 to value
	POP	R0,@R15		; Restore sign
	LDCTLB	FLAGS,RL0
	JR	C,SIN12
	CALL	INVSGN		; Negative - Make positive
SIN12:
	LD	R3,#SINTAB	; Coefficient table
	JP	SUMSER		; Evaluate sum of series
;
HALFPI:
	DB	0DBH,00FH,049H,081H	; 1.5708 (PI/2)
;
QUARTR:
	DB	000H,000H,000H,07FH	; 0.25
;
SINTAB:
	DB	5			; Table used by SIN
	DB	0BAH,0D7H,01EH,086H	; 39.711
	DB	064H,026H,099H,087H	;-76.575
	DB	058H,034H,023H,087H	; 81.602
	DB	0E0H,05DH,0A5H,086H	;-41.342
	DB	0DAH,00FH,049H,083H	; 6.2832
;
	ALIGN	2
TAN:
	CALL	STAKFP		; Put angle on stack
	CALL	SIN		; Get SIN of angle
	POP	R1,@R15		; Restore angle
	POP	R3,@R15
	CALL	STAKFP		; Save SIN of angle
	EX	R3,R2		; BCDE = Angle
	CALL	FPBCDE		; Angle to FPREG
	CALL	COS		; Get COS of angle
	JP	DIV		; TAN = SIN / COS
;
ATN:
	CALL	TSTSGN		; Test sign of value
	JR	PL,ATN1
	CALL	NEGAFT		; Negate result after if -ve
ATN1:
	JR	PL,ATN2
	CALL	INVSGN		; Negate value if -ve
ATN2:
	LDB	RH0,(FPEXP)	; Get exponent
	CPB	RH0,#81H	; Number less than 1?
	JP	C,ATN3		; Yes - Get arc tangnt
	LD	R1,#8100H	; BCDE = 1
	LDB	RH2,RL1
	LDB	RL2,RL1
	CALL	DVBCDE		; Get reciprocal of number
	LD	R3,#SUBPHL	; Sub angle from PI/2
	PUSH	@R15,R3		; Save for angle > 1
ATN3:
	LD	R3,#ATNTAB	; Coefficient table
	CALL	SUMSER		; Evaluate sum of series
	LD	R3,#HALFPI	; PI/2 - angle in case > 1
	RET			; Number > 1 - Sub from PI/2
;
ATNTAB:
	DB	9			; Table used by ATN
	DB	04AH,0D7H,03BH,078H	; 1/17
	DB	002H,06EH,084H,07BH	;-1/15
	DB	0FEH,0C1H,02FH,07CH	; 1/13
	DB	074H,031H,09AH,07DH	;-1/11
	DB	084H,03DH,05AH,07DH	; 1/9
	DB	0C8H,07FH,091H,07EH	;-1/7
	DB	0E4H,0BBH,04CH,07EH	; 1/5
	DB	06CH,0AAH,0AAH,07FH	;-1/3
	DB	000H,000H,000H,081H	; 1/1
;
	ALIGN	2
ARET:
	RET			; A RETurn instruction
;
GETINP:
	CALL	CONIN		;input a character
	RET
;
CLS:
	LDB	RH0,#CS		; ASCII Clear screen
	JP	MONOUT		; Output character
;
WIDTH:
	CALL	GETINT		; Get integer 0-255
	LDB	RH0,RL2		; Width to A
	LDB	(LWIDTH),RH0	; Set width
	RET
;
LINES:
	CALL	GETNUM		; Get a number
	CALL	DEINT		; Get integer -32768 to 32767
	LD	(LINESC),R2	; Set lines counter
	LD	(LINESN),R2	; Set lines number
	RET
;
DEEK:
	CALL	DEINT		; Get integer -32768 to 32767
	PUSH	@R15,R2		; Save number
	POP	R3,@R15		; Number to HL
	LDB	RH1,@R3		; Get LSB of contents
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	RH0,@R3		; Get MSB of contents
	JP	ABPASS		; Return integer AB
;
DOKE:
	CALL	GETNUM		; Get a number
	CALL	DEINT		; Get integer -32768 to 32767
	PUSH	@R15,R2		; Save address
	LD	RH4,#','
	CALL	CHKSYN		; Make sure ',' follows
	CALL	GETNUM		; Get a number
	CALL	DEINT		; Get integer -32768 to 32767
	EX	@R15,R3		; Save value,get address
	LDB	@R3,RL2		; Save LSB of value
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	LDB	@R3,RH2		; Save MSB of value
	POP	R3,@R15		; Restore code string address
	RET
;

; HEX$(nn) Convert 16 bit number to Hexadecimal string
;
HEX:
	CALL	TSTNUM		; Verify it's a number
	CALL	DEINT		; Get integer -32768 to 32767
	PUSH	@R15,R1		; Save contents of BC
	LD	R3,#PBUFF
	LDB	RH0,RH2		; Get high order into A
	CPB	RH0,#0
	JR	Z,HEX2		; Skip output if both high digits are zero
	CALL	BYT2ASC		; Convert D to ASCII
	LDB	RH0,RH1
	CPB	RH0,#'0'
	JR	Z,HEX1		; Don't store high digit if zero
	LDB	@R3,RH1		; Store it to PBUFF
	LDCTLB	RL4,FLAGS
	INC	R3		; Next location
	LDCTLB	FLAGS,RL4
HEX1:
	LDB	@R3,RL1		; Store C to PBUFF+1
	LDCTLB	RL4,FLAGS
	INC	R3		; Next location
	LDCTLB	FLAGS,RL4
HEX2:
	LDB	RH0,RL2		; Get lower byte
	CALL	BYT2ASC		; Convert E to ASCII
	LDB	RH0,RH2
	CPB	RH0,#0
	JR	NZ,HEX3		; If upper byte was not zero then always print lower byte
	LDB	RH0,RH1
	CPB	RH0,#'0'	; If high digit of lower byte is zero then don't print
	JR	Z,HEX4
HEX3:
	LDB	@R3,RH1		; to PBUFF+2
	LDCTLB	RL4,FLAGS
	INC	R3		; Next location
	LDCTLB	FLAGS,RL4
HEX4:
	LDB	@R3,RL1		; to PBUFF+3
	LDCTLB	RL4,FLAGS
	INC	R3		; PBUFF+4 to zero
	LDCTLB	FLAGS,RL4
	XORB	RH0,RH0
	RESFLG	C		; Terminating character
	LDB	@R3,RH0		; Store zero to terminate
	LDCTLB	RL4,FLAGS
	INC	R3		; Make sure PBUFF is terminated
	LDCTLB	FLAGS,RL4
	LDB	@R3,RH0		; Store the double zero there
	POP	R1,@R15		; Get BC back
	LD	R3,#PBUFF	; Reset to start of PBUFF
	JP	STR1		; Convert the PBUFF to a string and return it
;
BYT2ASC:
	LDB	RH1,RH0		; Save original value
	ANDB	RH0,#0FH	; Strip off upper nybble
	CPB	RH0,#0AH	; 0-9?
	JR	C,ADD30		; If A-F, add 7 more
	ADDB	RH0,#07H	; Bring value up to ASCII A-F
ADD30:
	ADDB	RH0,#'0'	; And make ASCII
	LDB	RL1,RH0		; Save converted char to C
	LDB	RH0,RH1		; Retrieve original value
	RRB	RH0,#1		; and Rotate it right
	RRB	RH0,#1
	RRB	RH0,#1
	RRB	RH0,#1
	ANDB	RH0,#0FH	; Mask off upper nybble
	CPB	RH0,#0AH	; 0-9? < A hex?
	JR	C,ADD301	; Skip Add 7
	ADDB	RH0,#07H	; Bring it up to ASCII A-F
ADD301:
	ADDB	RH0,#'0'	; And make it full ASCII
	LDB	RH1,RH0		; Store high order byte
	RET
;
; Convert "&Hnnnn" to FPREG
; Gets a character from (HL) checks for Hexadecimal ASCII numbers "&Hnnnn"
; Char is in A, NC if char is;<=>?@ A-z, CY is set if 0-9
HEXTFP:
	EX	R3,R2		; Move code string pointer to DE
	LD	R3,#0000H	; Zero out the value
	CALL	GETHEX		; Check the number for valid hex
	JP	C,HXERR		; First value wasn't hex, HX error
	JR	HEXLP1		; Convert first character
HEXLP:
	CALL	GETHEX		; Get second and addtional characters
	JR	C,HEXIT		; Exit if not a hex character
HEXLP1:
	ADD	R3,R3		; Rotate 4 bits to the left
	ADD	R3,R3
	ADD	R3,R3
	ADD	R3,R3
	ORB	RH0,RL3
	RESFLG	C		; Add in D0-D3 into L
	LDB	RL3,RH0		; Save new value
	JR	HEXLP		; And continue until all hex characters are in
;
GETHEX:
	LDCTLB	RL4,FLAGS
	INC	R2
	LDCTLB	FLAGS,RL4		; Next location
	LDB	RH0,@R2		; Load character at pointer
	CPB	RH0,#' '
	JP	Z,GETHEX	; Skip spaces
	SUBB	RH0,#30H		; Get absolute value
	RET	C		; < "0", error
	CPB	RH0,#0AH
	JR	C,NOSUB7	; Is already in the range 0-9
	SUBB	RH0,#07H	; Reduce to A-F
	CPB	RH0,#0AH	; Value should be $0A-$0F at this point
	RET	C		; CY set if was :
				; < = > ? @
NOSUB7:
	CPB	RH0,#10H	; > Greater than "F"?
	COMFLG	C
	RET			; CY set if it wasn't valid hex

HEXIT:
	EX	R3,R2		; Value into DE, Code string into HL
	LDB	RH0,RH2		; Load DE into AC
	LDB	RL1,RL2		; For prep to
	PUSH	@R15,R3
	CALL	ACPASS		; ACPASS to set AC as integer into FPREG
	POP	R3,@R15
	RET
;
HXERR:
	LDB	RL2,#HX		; ?HEX Error
	JP	ERROR
;
; BIN$(NN) Convert integer to a 1-16 char binary string
BIN:
	CALL	TSTNUM		; Verify it's a number
	CALL	DEINT		; Get integer -32768 to 32767
BIN2:
	PUSH	@R15,R1		; Save contents of BC
	LD	R3,#PBUFF
	LDB	RH1,#17		; One higher than max char count
ZEROSUP:
; Suppress leading zeros
	DECB	RH1,#1		; Max 16 chars
	LDB	RH0,RH1
	CPB	RH0,#01H
	JR	Z,BITOUT	; Always output at least one character
	RLCB	RL2,#1
	RLCB	RH2,#1
	JR	NC,ZEROSUP
	JR	BITOUT2
BITOUT:
	RLCB	RL2,#1
	RLCB	RH2,#1		; Top bit now in carry
BITOUT2:
	LDB	RH0,#'0'	; Char for '0'
	LDB	RL4,#0
	ADCB	RH0,RL4		; If carry set then '0' --> '1'
	LDB	@R3,RH0
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4
	DECB	RH1,#1
	JR	NZ,BITOUT
	XORB	RH0,RH0
	RESFLG	C		; Terminating character
	LDB	@R3,RH0		; Store zero to terminate
	LDCTLB	RL4,FLAGS
	INC	R3
	LDCTLB	FLAGS,RL4	; Make sure PBUFF is terminated
	LDB	@R3,RH0		; Store the double zero there
	POP	R1,@R15
	LD	R3,#PBUFF
	JP	STR1
;
; Convert "&Bnnnn" to FPREG
; Gets a character from (HL) checks for Binary ASCII numbers "&Bnnnn"
BINTFP:
	EX	R3,R2		; Move code string pointer to DE
	LD	R3,#0000H	; Zero out the value
	CALL	CHKBIN		; Check the number for valid bin
	JP	C,BINERR	; First value wasn't bin, HX error
BINIT:
	SUBB	RH0,#'0'
	ADD	R3,R3		; Rotate HL left
	ORB	RH0,RL3
	RESFLG	C
	LDB	RL3,RH0
	CALL	CHKBIN		; Get second and addtional characters
	JR	NC,BINIT	; Process if a bin character
	EX	R3,R2		; Value into DE, Code string into HL
	LDB	RH0,RH2		; Load DE into AC
	LDB	RL1,RL2		; For prep to
	PUSH	@R15,R3
	CALL	ACPASS		; ACPASS to set AC as integer into FPREG
	POP	R3,@R15
	RET
;
; Char is in A, NC if char is 0 or 1
CHKBIN:
	LDCTLB	RL4,FLAGS
	INC	R2
	LDCTLB	FLAGS,RL4
	LDB	RH0,@R2
	CPB	RH0,#' '
	JP	Z,CHKBIN	; Skip spaces
	CPB	RH0,#'0'	; Set C if < '0'
	RET	C
	CPB	RH0,#'2'
	COMFLG	C		; Set C if > '1'
	RET
;
BINERR:
	LDB	RL2,#BN		; ?BIN Error
	JP	ERROR
;
JJUMP1:
	JP	CSTART		; Go and initialise
;
MONOUT:
	JP	CONOUT		; output a char
;
MONITR:
	JP	3000H		; Restart (Normally Monitor Start)
;
INITST:
	LDB	RH0,#0		; Clear break flag
	LDB	(BRKFLG),RH0
	JP	INIT
;
TSTBIT:
	LDCTLB	RL0,FLAGS
	PUSH	@R15,R0		; Save bit mask
	ANDB	RH0,RH1
	RESFLG	C		; Get common bits
	POP	R1,@R15		; Restore bit mask
	CPB	RH0,RH1		; Same bit set?
	LDB	RH0,#0		; Return 0 in A
	RET
;
OUTNCR:
	CALL	OUTC		; Output character in A
	JP	PRCRLF		; Output CRLF
;
	END
