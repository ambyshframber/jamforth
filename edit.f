(
	EDITING TEXT ----------------------------------------------------------------------

	"Never put anything on disk you can't modify! And we haven't discussed how you get text on disk in the first place. Do not load it from cards! You're misdirecting your effort toward card reading, and you had to punch the cards anyway. Type it. The definitions required to edit the text stored in blocks (SCREENs) is simple."

	- Programming a Problem-Oriented Language, Chuck Moore, 1970

	A text editor is a pretty useful thing to have. The editor implemented here is very loosely based on the one Chuck Moore describes in his aforementioned book. It's based on a list of lines
)

VARIABLE CURSOR
VARIABLE #LINES
4096 2 CELLS * \DUP CONSTANT LINES_TOP
ALLOT CONSTANT LINES

VARIABLE #CONTEXT
2 #CONTEXT !

0 CONSTANT E0

: C? CURSOR @ 1+ . ;
: CPTR CURSOR @ 8 * LINES + ;

: MEMMOVE ( src dst len -- ) \ overlapping memcpy
	DUP >R SWAP >R
	HERE @ SWAP
	MEMCPY

	HERE @ R> R> MEMCPY
;

: GO 1- 0 MAX CURSOR ! ;

: RLIST ( start end )
	SWAP 1- 0 MAX
	SWAP 1-
	#LINES @ DUP UWIDTH -ROT
	MIN ROT
	BEGIN 2DUP <> WHILE
		( w #l idx )
		ROT 2DUP SWAP 1+ SWAP U.R SPACE '\t' EMIT
		-ROT
		DUP 8 * LINES + 2@ TELL
		1+
	REPEAT
	DROP 2DROP
;
: LIST
	1 #LINES 1+ RLIST
;

: T CPTR 2@ TELL ;
: TC
	CURSOR @ 1+ #CONTEXT @
	2DUP - -ROT +
	1+
	RLIST
;

: #
	CURSOR @ 1+
	#LINES @ MIN
	CURSOR !
;
: ~ CURSOR @ 1- 0 MAX CURSOR ! ;


: GETLINE
	HERE @
	BEGIN
		KEY 2DUP SWAP C!
		'\n' =
		SWAP 1+ SWAP
	UNTIL
	DUP HERE @ TUCK -
	ROT ALIGNED HERE !
;

: A
	GETLINE
	#LINES @ 8 * LINES + 2!
	1 #LINES +!
	#LINES @ CURSOR !
;
: R
	GETLINE
	CURSOR @ 8 * LINES + 2!
;

: INSERT
	1 CURSOR +!
	#LINES @ CURSOR @ - 2 *
	CPTR DUP 8 + ROT MEMMOVE
	1 #LINES +!
;

: I
	CURSOR @ #LINES @ = IF
		A
	ELSE
		INSERT R
	THEN
;

: K
	1 #LINES -!
	#LINES @ CURSOR @ - 2 *
	( len )
	CPTR DUP 8 + SWAP ROT MEMCPY
	~
;

: CLEAR
	0 CURSOR !
	0 #LINES !
	E0 HERE !
;

: DEFRAG \ rebuild contiguous file
	HERE @
	#LINES @ 0
	BEGIN 2DUP <> WHILE
		DUP 8 * LINES + 2@ HERE @ SWAP DUP >R
		( #lines idx src dst len )
		\.S CR
		CMEMCPY
		R> HERE +!
		1+
	REPEAT
	2DROP
	DUP HERE @ SWAP - E0 SWAP
	\.S CR
	DUP >R CMEMCPY R>
;

: RESPLIT ( addr u )
	CLEAR
	DUP HERE +! ALIGN
	
	OVER + OVER ( start end start )

	BEGIN 2DUP <> WHILE
		( line_start text_end idx )
		DUP C@ '\n' = IF
			1+
			ROT 2DUP -
			( tend idx ls len )
			CPTR 2!
			DUP -ROT
			1 CURSOR +!
			1 #LINES +!
		ELSE
			1+
		THEN
	REPEAT
	DROP 2DROP
;

: J DEFRAG E0 SWAP RESPLIT ;

: STR
	J
	E0
	#LINES @ 1- 8 * LINES +
	2@ + OVER -
;

: (FILE>)
	CLEAR
	R/O OPEN-FILE ?DUP IF
		S" (FILE>): failed to open for reading" PERROR
		DROP EXIT
	THEN
	DUP FSTAT ?DUP IF
		S" (FILE>): failed to fstat" PERROR
	THEN
	HERE @ STAT_SIZE
	\.S CR
	DUP 2 * UNUSED [ 2 1024 * ], - > IF \ if loading and defragging would leave less than two pages unused
		DUP 2 * PAGEALIGNED 4 /
		\." allocate " DUP . ." cells" CR
		MORECORE ?DUP IF
			S" (FILE>): failed to allocate" PERROR
		THEN
	THEN

	( fd len )
	2DUP
	E0 SWAP ROT READ-FILE ?DUP IF
		S" (FILE>): failed to read" PERROR
	THEN
	HERE +! ALIGN
	SWAP CLOSE-FILE DROP
	E0 SWAP RESPLIT
;

: FILE>
	WORD (FILE>)
;

: (>FILE)
	O_WRONLY CREATE-FILE ?DUP IF
		S" (FILE>): failed to open for writing with truncate" PERROR
		DROP EXIT
	THEN
	DUP
	STR ROT WRITE-FILE ?DUP IF
		S" (>FILE): failed to write" PERROR
	THEN
	DROP CLOSE-FILE DROP
;

: >FILE WORD (>FILE) ;

LOADED edit.f
HERE @ ' E0 8 + !
