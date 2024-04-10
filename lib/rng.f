DECIMAL

S" /dev/urandom" R/O OPEN-FILE DROP
CONSTANT URANDOM_FD

512 ALLOT CONSTANT RANDOM_BUF

VARIABLE RANDOM_BUF_IDX

: REFILL_RANDOM
	RANDOM_BUF 512 URANDOM_FD READ-FILE 2DROP
;
REFILL_RANDOM

: (RANDOM)
	RANDOM_BUF_IDX DUP @ DUP ( *rbi rbi rbi )
	RANDOM_BUF + @ ( *rbi rbi v )
	\.S CR
	-ROT 4+ DUP 512 >= IF
		\." refill random buffer"
		REFILL_RANDOM
		DROP 0 SWAP !
	ELSE
		SWAP !
	THEN
;

2 CELLS ALLOT CONSTANT XORSHIFT64

: CYCLE_XORSHIFT
	XORSHIFT64 2@ 2DUP
	( lo hi lo hi )
	13 << SWAP DUP 13 << SWAP 19 >>
	( lo hi hi<<13 lo<<13 lo>>19 )
	ROT OR
	( lo hi lo<<13 hi<<13c )
;

( dice words for a thing i did once )

: DICE0 ( A B -- BdA ) \ zero indexed dice
	0 SWAP ( A acc B )
	DFOR
		>R OVER ( A acc A )
		(RANDOM) SWAP UMOD ( A acc roll )
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
