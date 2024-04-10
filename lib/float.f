(
	FLOATING POINT, CONTINUED ----------------------------------------------------------------------

	Note: read the floating point section in jonesforth.S first.

	x87 stack manipulation words are all well and good, but when it comes to ergonomics, they are somewhat lacking. Also, being able to parse and print floating point numbers is pretty useful for doing really anything with them.

	A lot of these words are quite hard to follow because they juggle values between two (and in one case all three) stacks.

	First off, we define equivalents to LITERAL and ], for the x87 stack. These are based on XLIT, and as such push values directly to the float stack.

	Next, we define some MKXD_ words ("make X D ___"). These are what create our nice neat floats-on-data-stack words. See if you can figure out how they work!
)

: XLITERAL IMMEDIATE ' XLIT , ,	;
: ]X, ] [COMPILE] XLITERAL ;

: MKXDBC \ binary commutative
	WORD 2DUP COUNTC DROP CREATE DOCOL ,
	' >X , ' >X , FIND >CFA , ' X> , ' EXIT ,
;
: MKXDBN \ binary non-commutative
	WORD 2DUP COUNTC DROP CREATE DOCOL ,
	' >X , ' >X , ' XSWAP , FIND >CFA , ' X> , ' EXIT ,
;
: MKXDU \ unary
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

(
	These are some useful utility words that turn up later.

	TODO: make FSIGNUM correct
)

: FSIGNUM [ HEX 80000000 DEC ], AND 31 >> NOT ;

: XF**I ( D: b -- , F: a -- a**b )
	XF1 DFOR
		XOVER XF*
	REPEAT
	XSWAP XDROP
;
: F**I ( a b -- a**b )
	SWAP >X XF**I X>
;

(
	x87 actually provides instructions for loading zero and one, so it would be a shame not to use them.
)

: F0 XF0 X> ;
: F1 XF1 X> ;
: FPI XFPI X> ;

(
	I wanted to use >F and F> for these conversion words, but unfortunately those names were needed for comparison words.
)

: F>I >X X>I ;
: F>T >X X>IT ;
: I>F I>X X> ;

(
	PARSING FLOATS ----------------------------------------------------------------------

	It turns out it's not that hard to parse floating point numbers if you're not bothered about being completely correct. The following definitions will work fine for numbers between -2^31 and 2^31, but completely crap out outside that. Sidenote: if you ever end up writing your own float parse/print routines, pay special attention to the range (-1,0). It can cause no end of trouble.

	Using a similar trick to KEYCB, NUMBER can be replaced at runtime. This means you can type floating point numbers directly into the prompt and it Just Works.
)

: XFLT ( D: int frac p -- , X: -- f )
	ROT I>X \ int part on x87 stack
	( frac prec )
	SWAP I>X
	BASE @ I>X XF**I XF/ XF+
;
: FLT XFLT X> ;

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

\ "sf" here means "standard form"
: XFSF> ( D: i -- , X: f -- f )
	BASE @ I>X XF**I XF*
;
: FSF> ( f i -- f )
	SWAP >X XFSF> X>
;

(
	PRINTING FLOATS ----------------------------------------------------------------------

	Printing is also not that hard if you don't care about correctness. I'll write F.SF at some point, I promise.

	The simplest way is to split the number into integer and fraction parts, then print the int part followed by a dot. Then, multiply the frac part by the desired amount of precision and print it (zero-padded). A more complex but more useful way is to determine the required precision from the size of the number, using logarithms.

	Something to note is that the parse/print routines in this file are base-agnostic, which isn't something I've seen in any other language.
)

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

: XF.P ( D: rel_recision -- , X: x -- )
	XDUP XFABS BASE @ I>X XFLOGN ( X: x log_b(x) )
	X>IT NEGATE + 1 MAX XF.A
;
: F.P SWAP >X XF.P ;

HEX 4b800000 DEC CONSTANT 2**24
: F. 2**24 BASE @ I>F FLOGN F>T F.P SPACE ;

(
	COMPARISONS ----------------------------------------------------------------------

	Arithmetic is all well and good, but comparisons are pretty useful for doing real work. I deliberately do not define F= here, because that way lies madness. You're welcome to define it yourself but don't say I didn't warn you.
)

: XF<
	XFCMP FSTSW
	[ BIN 0100010100000000 DEC ],
	AND 0=
;
: XF> XF< NOT ;

: F< >X >X XF< NOT ; \ order on stack swaps
: F> >X >X XF< ;

: X.S
	8 DFOR
		XEMPTY? UNLESS
			XDUP> HU.
		THEN
		XINCSTP
	REPEAT
;
: XF.S
	0 BEGIN
		DUP 8 <> XEMPTY? NOT AND
	WHILE
		DUP CELLS HERE @ + X!
		1+
	REPEAT
	( xdepth )
	0 BEGIN 2DUP <> WHILE
		DUP CELLS HERE @ +
		@ F.
		1+
	REPEAT
	DROP
	0 BEGIN 2DUP <> WHILE
		2DUP - 1- CELLS HERE @ +
		@ >X
		1+
	REPEAT
	2DROP
;


LOADED float.f
