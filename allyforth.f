: [CHAR] IMMEDIATE CHAR [COMPILE] LITERAL ;

: LOOKUPXT WORD FIND >CFA ;

: COUNT ( addr len -- addr len first )
	( get the first character of a string and advance the string pointer )
	OVER C@
	ROT 1+
	ROT 1-
	ROT
;
: COUNTC COUNT ;
: COUNT4 ( addr len -- addr len first )
	\ like COUNT but with cell-sized data
	OVER @ ROT 4+ ROT 1- ROT
;


: MAX ( a b -- max )
	2DUP > IFELSE DROP NIP
;
: MIN ( a b -- min )
	2DUP < IFELSE DROP NIP
;

: BINARY 2 BASE ! ;
: BIN BINARY ;
: DEC 10 BASE ! ;

: H. BASE @ HEX SWAP . BASE ! ; \ useful for debugging
: HU. BASE @ HEX SWAP U. BASE ! ;

: '\r' 13 ;
: '\t' 9 ;

: is_whitespace
	DUP BL =
	SWAP DUP '\r' =
	SWAP DUP '\n' =
	SWAP '\t' =
	OR OR OR
;

: scrub_space ( addr len -- a2 l2 a1 l1 )
	BEGIN COUNT is_whitespace NOT UNTIL \ skip starting whitespace
	SWAP 1- SWAP 1+ \ roll back a character

	OVER -ROT
	BEGIN COUNT is_whitespace UNTIL \ find next whitespace
	( old_a rem_a rem_len )
	
	\ a1 = old_a
	\ l1 = rem_a old_a - 1-

	-ROT OVER >R \ puts a1 on return stack
	TUCK SWAP - 1- >R \ puts l1 on return stack
	SWAP

	BEGIN COUNT is_whitespace NOT UNTIL \ skip ending whitespace
	SWAP 1- SWAP 1+ \ roll back a character

	R> R> SWAP
;

\ string equality
: S= ( a1 l1 a2 l2 -- 1 if equal | 0 if not )
	ROT OVER ( a1 a2 l2 l1 l2 )
	= UNLESS
		2DROP DROP 0 EXIT \ length doesn't match
	THEN
	( a1 a2 length )
	BEGIN DUP WHILE
		-ROT 2DUP C@ SWAP C@ ( length a1 a2 c1 c2 )
		= UNLESS 
			2DROP DROP 0 EXIT \ character didn't match
		THEN
		1+ SWAP 1+ ROT 1-
	REPEAT
	2DROP DROP 1 \ strings were equal
;

: (FORGET) \ forget based on pointer, not name
	DUP @ LATEST !
	HERE !
; \ TODO make this check LATEST points to a word

: ;TMP IMMEDIATE
	LATEST @ [COMPILE] LITERAL
	' (FORGET) ,
	[COMPILE] ;
;

: ], ] [COMPILE] LITERAL ;

(
\ simple non-secure random number generator
\ because it's so simple, it's also shit
: RANDOMC RDTSC DROP DUP 4 >> SWAP 12 >> XOR 255 AND ;

\ less simple and less shit rng based on an lfsr and the timestamp counter
VARIABLE LFSR
\RANDOMC RANDOMC 8 << OR 1 OR LFSR ! \ 1 or makes sure it doesn't get filled with all zeros
: ADVANCE_LFSR
	LFSR @
	DUP 7 >> XOR 65535 AND
	DUP 9 << XOR 65535 AND
	DUP 13 >> XOR 65535 AND
	LFSR !
;
: RAND2
	LFSR @ 255 AND
	\ advance the lfsr between 1 and 4 times, using a different source of randomness
	RANDOMC 3 AND 1+ BEGIN ?DUP WHILE 1- ADVANCE_LFSR REPEAT
;

: RANDOM \ TODO make this less deep on the stack
	RAND2 RAND2 RAND2 RAND2
	8 << OR 8 << OR 8 << OR  
;
)

\ better rng that just uses /dev/urandom

S" /dev/urandom" R/O OPEN-FILE DROP
CONSTANT URANDOM-FD

DECIMAL

512 ALLOT CONSTANT RANDOM-BUF

VARIABLE RANDOM-BUF-IDX

: READ-RANDOM
	RANDOM-BUF 512 URANDOM-FD READ-FILE 2DROP
;
READ-RANDOM

: RANDOM
	RANDOM-BUF-IDX DUP @ DUP ( *rbi rbi rbi )
	RANDOM-BUF + @ ( *rbi rbi v )
	.S CR
	-ROT 4+ DUP 512 >= IF
		." refill random buffer"
		READ-RANDOM
		DROP 0 SWAP !
	ELSE
		SWAP !
	THEN
;

(
	IMMEDIATE IF ----------------------------------------------------------------------

	The eponymous Jones has left us some homework: "Making [the control structures] work in immediate mode is left as an exercise for the reader."

	The solution I thought of is to compile a temporary word, EXECUTE it, then FORGET it. This requires modifications to IF and THEN, but not ELSE. The main difference is an additional value left on the stack, underneath the address of the 0BRANCH word that gets compiled. This additional value tells THEN whether to compile as normal, or end compilation and execute the word. Conveniently, we can use the xt returned by :NONAME as a flag.

	It should go without saying that you shouldn't use this for conditional compilation.
)

: IF IMMEDIATE
	STATE @ IF \ compiling
		0 \ leave flag for THEN
	ELSE \ immediate
		:NONAME \ start compiling an anonymous word
	THEN
	[COMPILE] IF
;

: THEN IMMEDIATE
	[COMPILE] THEN
	
	?DUP IF \ check flag/xt left by IF
		' EXIT , \ finish off word
		EXECUTE \ execute it
		
		LATEST @ DUP @ LATEST !
		HERE ! \ make sure it doesn't leak

		[COMPILE] [
	THEN
;
DROP \ because the above definition uses the new version of if, it leaves the flag on the stack

: UNLESS IMMEDIATE
	STATE @ IF \ decide whether to execute or compile NOT based on STATE
		' NOT ,
	ELSE
		NOT
	THEN
	[COMPILE] IF
;

(
	IMMEDIATE IFTHEN

	IFTHEN is a control flow word I wrote to make certain conditionals slightly smaller. It goes hand in hand with ULTHEN and IFELSE.
)

: IFTHEN IMMEDIATE
	STATE @ IF
		' IFTHEN ,
	ELSE
		LOOKUPXT SWAP IFELSE EXECUTE DROP
	THEN
;
: ULTHEN IMMEDIATE
	STATE @ IF
		' ULTHEN ,
	ELSE
		LOOKUPXT SWAP IFELSE DROP EXECUTE
	THEN
;

: IFELSE IMMEDIATE
	STATE @ IF
		' IFELSE ,
	ELSE
		LOOKUPXT LOOKUPXT ROT ULTHEN SWAP DROP EXECUTE
	THEN
;

(
	LOOP WORDS

	I got bored of doing BEGIN ?DUP WHILE all the time so I wrote myself some better loop words.
)

: DFOR IMMEDIATE
	[COMPILE] BEGIN
	' ?DUP ,
	[COMPILE] WHILE
	' 1- ,
;

: ** ( a b -- a**b )
	1 SWAP ( a 1 b )
	DFOR
		-ROT OVER * ROT
	REPEAT
	( a a**b )
	NIP
;


(
	INLINE LAMBDAS

	Not really lambdas, but the name is cool.

	CURRENTLY BROKEN
)

: LAMBDA IMMEDIATE
	' BRANCH ,
	0 ,
	HERE @
	DUP 4-
	.S CR
	DOCOL ,
;
: ADBMAL IMMEDIATE
	' EXIT ,
	HERE @ . CR
	.S
	DUP
	HERE @ SWAP -	\ calculate the offset from the address saved on the stack
	SWAP !
	[COMPILE] LITERAL
;

( dice words for a thing i did once )

: DICE0 ( A B -- BdA ) \ zero indexed dice
	0 SWAP ( A acc B )
	DFOR
		>R OVER ( A acc A )
		RANDOM SWAP UMOD ( A acc roll )
		+ R>
	REPEAT
	NIP
;

: DICE1 ( A B -- BdA ) \ 1 indexed dice
	TUCK DICE0 +
;

: 1DICE0 1 DICE0 ;
: 1DICE1 1 DICE1 ;

(
	BETTER DECOMPILATION ----------------------------------------------------------------------

	Jonesforth's SEE segfaults if it encounters a word it doesn't understand, which isn't great. With certain gnarlier dictionary arrangements (not really that gnarly - a simple ALLOT or VARIABLE can break SEE for the previous word) it's completely useless. So, I've made a slightly less failure-prone but also less ergonomic decompiler.

	DC-CELL takes a 32-bit value and prints its signed decimal and unsigned hex values, its value when interpreted as a 4-character ASCII string (replacing non-printing characters with "."), and its name (or "?").
)

37 ALLOT CONSTANT SFIGS
ALIGN
: GEN-SFIGS
	35 DFOR
		DUP 2 +
		DUP BASE !
		[ HEX ffffffff ], UWIDTH
		SWAP SFIGS + C!
	REPEAT
;
GEN-SFIGS
DEC
FORGET GEN-SFIGS

: DC-CELL ( v )
	BASE @ SWAP

	DUP HEX 8 U.R
	DUP DECIMAL 12 .R
	(
	( b v )
	SWAP DUP BASE !
	DUP DUP 16 = SWAP 10 = OR UNLESS
		SFIGS + C@ 2 + .R
	THEN
	)
	SWAP BASE !
	DUP
	
	SPACE

	\ little endian - so first character is the low byte
	4 DFOR
		SWAP
		[ DECIMAL ] DUP 255 AND
		DUP 32 126 WITHIN
		UNLESS DROP [CHAR] . THEN EMIT
		8 >> SWAP
	REPEAT
	DROP SPACE

	DUP LATEST @ < IF
		CFA> ?DUP IF ID. ELSE
			[CHAR] ? EMIT
		THEN
	ELSE
		DROP [CHAR] ? EMIT
	THEN

	CR
;

: DC-CELLS ( addr len ) \ len is number of cells, not number of bytes
	BEGIN ?DUP WHILE
		COUNT4 DC-CELL
	REPEAT
;

: DC-REP ( addr )
	BEGIN
		DUP @ DC-CELL
		4+
		KEY '\n' <>
	UNTIL
	DROP
;

(
	MODULES ----------------------------------------------------------------------

	This is a very silly and pretty unnecessary feature. Basically I thought "what if I wanna give the same simple name to multiple words" and then that led to "i can make the dictionary skip a section by frobbing the link pointers" and now we're here.
)

VARIABLE LATESTMOD

: FINDMOD ( a l -- *mod|0 )
	LATESTMOD
	BEGIN ?DUP WHILE
		DUP DUP >R 16 + C@ 63 AND SWAP 17 + SWAP ( sa sl na nl )
		2SWAP 2DUP >R >R
		S= IF
			RDROP RDROP R> EXIT
		THEN
		R> R> R> @
	REPEAT
	2DROP 0
;

(
	module descriptor "struct"

	0	backpointer to previous module OR null
	4	word to be selected when enabled
	8	word to be selected when disabled
	12	word to have its backpointer changed ("guard word")
	16	namelen + flags (1 byte)
	17	name


	namelen + flags
	bits [5,0] = length
	bit 6 = module enabled (1 when in, 0 when out)
)


(
	EVALUATE ----------------------------------------------------------------------

	EVALUATE is a word that, given a string, evaluates it. Sounds pretty simple, right? Just iterate over the string and pass each word in turn to (INTERPRET). Well.

	There are many words that call WORD or KEY, such as :, .", S", etc etc. These words will behave oddly when EVALUATEd in this naive fashion, because KEY will still return characters from standard input. The way I chose to solve this is to temporarily override KEY (see jonesforth.S). The short explanation is that you write a callback to KEYCB, which is then called instead of KEY. This callback then needs to remove itself when it's finished, by writing 0 to KEYCB.

	EVAL_RECORD
		0	addr
		4	len
		8	read_idx
		12	refill

	refill is either an xt (hence bits 0 and 1 are clear), OR a file descriptor shifted left 2 and ORed with 1. If it's a function, the function is executed. If it's an fd the buffer is refilled by reading from it.

	Jonesforth's base KEY can actually be implemented in this fashion. 

	To allow for nested EVALUATEs, there's a stack of strings and callbacks called EVAL_RECORDS. The callbacks (if present) are executed when the string they are paired with is empty.
)

16 DUP CONSTANT MAX_EVAL_RECORDS
4 * CELLS ALLOT CONSTANT EVAL_RECORDS \ 16 levels of nesting
VARIABLE EVAL_RECORDS_IDX \ record index, not byte index. mul by 4 cells before adding to EVAL_RECORDS

-1 EVAL_RECORDS_IDX ! \ init to -1, because zero means 1 record

: KEY-FROM-BUF ( buf** -- c )
	DUP @ OVER 4+ @ ( buf** addr len )
	COUNTC >R
	ROT SWAP OVER ( addr buf** len buf** )
	4+ ! !
	R>
;

: KEY-FROM-RECS
	\ get the current record
	EVAL_RECORDS EVAL_RECORDS_IDX @ [ 4 CELLS ], * + DUP
	KEY-FROM-BUF ( buf** c )
	SWAP 4+ @ ( c len )
	UNLESS \ if length == 0, decrement idx by 1 and maybe go back to regular KEY
		EVAL_RECORDS_IDX @ UNLESS
			0 KEYCB !
		THEN
		1 EVAL_RECORDS_IDX -!
	THEN
;

: ADD-EVAL-REC ( addr len -- 0 | 1 ) \ 1 on failure
	EVAL_RECORDS_IDX @ [ MAX_EVAL_RECORDS 1- ], >= IF
		2DROP 1
	ELSE
		EVAL_RECORDS_IDX 1 SWAP +!
		\." adding rec"
		EVAL_RECORDS EVAL_RECORDS_IDX @ [ 4 CELLS ], * +
		\.S CR
		SWAP OVER 4+ ! !
		0
	THEN
;

: EVALUATE ( addr len )
	ADD-EVAL-REC
	IF
		\ just fucking panic back to the prompt
		." maximum EVALUATE nesting depth reached"
		0 KEYCB !
		-1 EVAL_RECORDS_IDX !
		QUIT
	THEN
	' KEY-FROM-RECS KEYCB !
;


(
	FLOATING POINT, CONTINUED ----------------------------------------------------------------------
)

: XF**I ( D: b -- , F: a -- a**b )
	XF1 DFOR
		XOVER XF*
	REPEAT
	XSWAP XDROP
;

: XFLT ( D: int frac p -- , X: -- f )
	ROT I>X \ int part on x87 stack
	( frac prec )
	SWAP I>X
	BASE @ I>X XF**I XF/ XF+
;
: FLT XFLT X> ;

: XF.A ( D: abs_precision -- , X: x -- )
	XDUP X>
	[ HEX 80000000 DEC ], AND IF [CHAR] - EMIT THEN
	DUP XFABS
	XFSPLIT 0 .R
	[CHAR] . EMIT
	BASE @ I>X XF**I
	XF* X>I SWAP U.RZ
;
: F.A SWAP >X XF.A ;

: MKXDBC
	WORD 2DUP COUNTC DROP CREATE DOCOL ,
	' >X , ' >X , FIND >CFA , ' X> , ' EXIT ,
;
: MKXDBN
	WORD 2DUP COUNTC DROP CREATE DOCOL ,
	' >X , ' >X , ' XSWAP , FIND >CFA , ' X> , ' EXIT ,
;
: MKXDU
	WORD 2DUP COUNTC DROP CREATE DOCOL ,
	' >X , FIND >CFA , ' X> , ' EXIT ,
;

MKXDBC XF+
MKXDBC XF*
MKXDBN XF-
MKXDBN XF/

MKXDU XF1/
MKXDU XFSPLIT
MKXDU XFNEGATE
MKXDU XFABS

MKXDU XFLOG2
MKXDBN XFLOGN

: FSIGNUM [ HEX 80000000 DEC ], AND 31 >> NOT ;

: F**I ( a b -- a**b )
	SWAP >X XF**I X>
;

: F0 XF0 X> ;
: F1 XF1 X> ;

: F>I >X X>I ;
: F>T >X X>IT ;
: I>F I>X X> ;

: XF.P ( D: rel_recision -- , X: x -- )
	XDUP XFABS BASE @ I>X XFLOGN ( X: x log_b(x) )
	X>IT NEGATE + 1 MAX XF.A
;
: F.P SWAP >X XF.P ;

HEX 4b800000 DEC CONSTANT 2**24
: F. 2**24 BASE @ I>F FLOGN F>T F.P SPACE ;

: INTEGER NUMBER ;

: NUMBER ( a l -- n x ) \ x is number of unparsed characters
	OVER C@ [CHAR] - = IF \ do negativeness manually
		COUNTC DROP
		1
	ELSE
		0
	THEN
	-ROT

	2DUP NUMBER ( neg a l n x )
	?DUP UNLESS
		( neg a l n )
		\ number parsed fully
		-ROT 2DROP SWAP IFTHEN NEGATE 0 EXIT
	THEN

	SWAP BASE @ / >R \ correct and stash value
	( neg a l1 l2 )
	TUCK - ( neg a l2 y )

	ROT + DUP C@ [CHAR] . = UNLESS
		ROT DROP
		DROP R> SWAP EXIT
	THEN

	SWAP COUNTC DROP ( neg a l )

	?DUP UNLESS \ length of frac part is 0
		DROP R> SWAP IFTHEN NEGATE I>F 0 EXIT
	THEN

	DUP -ROT NUMBER ( neg l n2 x )

	?DUP UNLESS \ fully parsed
		( neg l n2 )
		SWAP R> -ROT FLT SWAP IFTHEN FNEGATE 0 EXIT
	THEN

	( neg l n2 x )
	ROT DROP ROT DROP RDROP
;
LATEST @ >CFA NUMBERCB !

: XFSF> ( D: i -- , X: f -- f )
	BASE @ I>X XF**I XF*
;
: FSF> ( f i -- f )
	SWAP >X XFSF> X>
;

: # WORD NUMBER DROP ;
: H# BASE @ HEX # SWAP BASE ! ;

: XLITERAL IMMEDIATE ' XLIT , ,	;
: ]X, ] [COMPILE] XLITERAL ;

: XF<
	XFCMP FSTSW
	[ BIN 0100010100000000 DEC ],
	AND 0=
;
: XF> XF< NOT ;

: F< >X >X XF< NOT ; \ order on stack swaps
: F> >X >X XF< ;

S" /dev/stdin" R/O OPEN-FILE DROP


HEX
HERE @ SWAP
, 1 , \ fd 0, watch for POLLIN
CONSTANT STDIN_POLLFD

DUP CONSTANT DATA_SEG_START

DEC
HERE @ SWAP - DUP
." forth code used " . ." bytes (" 4 / . ." cells)" CR

GET-BRK HERE @ -
DUP . ." bytes (" 4 / . ." cells) remaining" CR
." OK" CR


