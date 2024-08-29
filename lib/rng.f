DECIMAL

4 CELLS ALLOT CONSTANT XORSHIFT128

\ i know i should really use getrandom here but I Can't Be Fucked
S" /dev/urandom" R/O OPEN-FILE DROP DUP
XORSHIFT128 4 CELLS ROT READ-FILE 2DROP
CLOSE-FILE DROP

DECIMAL

: CYCLE_XORSHIFT
	XORSHIFT128 DUP
	3 CELLS + @
	SWAP @
	( t s )
	XORSHIFT128 HERE @ 3 MEMCPY \ 32 bit shift
	HERE @ XORSHIFT128 4+ 3 MEMCPY

	SWAP
	DUP 11 << XOR DUP 8 >> XOR ( s t )
	OVER 19 >> XOR XOR
	XORSHIFT128 !
;

: RANDOM XORSHIFT128 @ CYCLE_XORSHIFT ;

( dice words for a thing i did once )

: DICE0 ( A B -- BdA ) \ zero indexed dice
	0 SWAP ( A acc B )
	DFOR
		>R OVER ( A acc A )
		RANDOM SWAP .S CR UMOD ( A acc roll )
		+ R>
	REPEAT
	NIP
;

: DICE1 ( A B -- BdA ) \ 1 indexed dice
	TUCK DICE0 +
;

: 1DICE0 1 DICE0 ;
: 1DICE1 1 DICE1 ;

: D DICE1 ;

LOADED rng.f
