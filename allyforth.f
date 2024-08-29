: [CHAR] IMMEDIATE CHAR [COMPILE] LITERAL ;

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

: @4+ ( p -- p+4 *p ) \ post-increment fetch
	DUP 4+ SWAP @
;

: 2@ DUP @ SWAP 4+ @ ;
: 2? 2@ SWAP . . ;
: 2! TUCK 4+ ! ! ;

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

: # IMMEDIATE
	WORD NUMBER DROP
	STATE @ IF [COMPILE] LITERAL THEN
;
: H# IMMEDIATE
	BASE @ HEX
	[COMPILE] #
	STATE @ ULTHEN SWAP
	BASE !
;
: B# IMMEDIATE
	BASE @ BINARY
	[COMPILE] #
	STATE @ ULTHEN SWAP
	BASE !
;

: '\r' 13 ;
: '\t' 9 ;

: SPACE=
	DUP BL =
	SWAP DUP '\r' =
	SWAP DUP '\n' =
	SWAP '\t' =
	OR OR OR
;

: SPLIT_SPACE ( addr len -- a2 l2 a1 l1 )
	BEGIN COUNT SPACE= NOT UNTIL \ skip starting whitespace
	SWAP 1- SWAP 1+ \ roll back a character

	OVER -ROT
	BEGIN COUNT SPACE= UNTIL \ find next whitespace
	( old_a rem_a rem_len )
	
	\ a1 = old_a
	\ l1 = rem_a old_a - 1-

	-ROT OVER >R \ puts a1 on return stack
	TUCK SWAP - 1- >R \ puts l1 on return stack
	SWAP

	BEGIN COUNT SPACE= NOT UNTIL \ skip ending whitespace
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

: ZS= ( ca a u -- eq? ) \ ca is a cstr
	BEGIN DUP WHILE
		-ROT
		OVER C@ ( u ca a cc )
		?DUP UNLESS \ is null
			DROP 2DROP 0 EXIT
		THEN
		OVER C@ = UNLESS
			DROP 2DROP 0 EXIT
		THEN
		( u ca a )
		1+ SWAP 1+ SWAP ROT 1-
	REPEAT
	( ca a u )
	2DROP @ 0=
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

: S' WORD FIND >CFA ;
: ' IMMEDIATE
	STATE @ IF
		' ' ,
	ELSE
		S'
	THEN
;

(
	IMMEDIATE IFTHEN

	IFTHEN is a control flow word I wrote to make certain conditionals slightly smaller. It goes hand in hand with ULTHEN and IFELSE.
)

: IFTHEN IMMEDIATE
	STATE @ IF
		' IFTHEN ,
	ELSE
		S' SWAP IFELSE EXECUTE DROP
	THEN
;
: ULTHEN IMMEDIATE
	STATE @ IF
		' ULTHEN ,
	ELSE
		S' SWAP IFELSE DROP EXECUTE
	THEN
;

: IFELSE IMMEDIATE
	STATE @ IF
		' IFELSE ,
	ELSE
		S' S' ROT ULTHEN SWAP DROP EXECUTE
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

(
	BETTER DECOMPILATION ----------------------------------------------------------------------

	Jonesforth's SEE segfaults if it encounters a word it doesn't understand, which isn't great. With certain gnarlier dictionary arrangements (not really that gnarly - a simple ALLOT or VARIABLE can break SEE for the previous word) it's completely useless. So, I've made a slightly less failure-prone but also less ergonomic decompiler.

	DC-CELL takes a 32-bit value and prints its signed decimal and unsigned hex values, its value when interpreted as a 4-character ASCII string (replacing non-printing characters with "."), and its name (or "?").
)

37 ALLOT CONSTANT SFIGS
ALIGN
: GEN_SFIGS
	35 DFOR
		DUP 2 +
		DUP BASE !
		[ HEX ffffffff ], UWIDTH
		SWAP SFIGS + C!
	REPEAT
;
GEN_SFIGS
DEC
FORGET GEN_SFIGS

: DC_CELL ( v )
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

: DC_CELLS ( addr len ) \ len is number of cells, not number of bytes
	BASE @ -ROT HEX 
	BEGIN ?DUP WHILE
		OVER 8 U.R SPACE
		COUNT4 DC_CELL
	REPEAT
	DROP BASE !
;

: DC_REP ( addr )
	BEGIN
		DUP @ DC_CELL
		4+
		KEY '\n' <>
	UNTIL
	DROP
;

(
	INLINING ----------------------------------------------------------------------

	Inlining entire Forth words is harder than it looks. It's practically impossible to know when a Forth word ends without having something in the header that tells you. Which unfortunately, we don't.

	So, I'm not going to try. Instead, I provide ways to inline specific sequences of words using [[ and ]]. Where you would go "' A , ' B ," you can instead go "[[ A B ]]". Much clearer.
)

: ([[)
	R> @4+ ( p len )
	DFOR
		SWAP @4+ , SWAP
	REPEAT
	>R
;
: [[ IMMEDIATE
	' ([[) ,
	HERE @
	0 ,
;
: ]] IMMEDIATE
	DUP
	HERE @ SWAP - 2 >> 1-
	SWAP !
;

(
	THE ENVIRONMENT, CONTINUED ----------------------------------------------------------------------

	Jones provides us with the ability to fetch arguments, but not environment variables. The environ is a null-terminated array of pointers to cstrings of the form "NAME=VALUE". I know, it sucks.
	
	ENV= compares a string "NAME" to a cstring "NAME=.*". Hopefully you can see why this is useful. ENVVAR checks every environment variable in this manner, then if it finds one that matches, returns the value as a string.

	I also define ZTELL here, because it was useful for debugging ENV= and ENVVAR and I thought it was worth keeping around once I was done.
)

: ZTELL DUP STRLEN TELL ;

: ENV= ( ca a u -- 1|0 )
	\.S CR
	BEGIN DUP WHILE
		-ROT
		OVER C@ ( u ca a cc )
		?DUP UNLESS \ is null
			DROP 2DROP 0 EXIT
		THEN
		OVER C@ = UNLESS
			DROP 2DROP 0 EXIT
		THEN
		( u ca a )
		1+ SWAP 1+ SWAP ROT 1-
	REPEAT
	\." names ="
	( ca a u )
	2DROP C@ [CHAR] = =
;

: ENVVAR ( a u -- a u (if var exists) | 0 0 (if it doesn't) )
	ENVIRON
	BEGIN DUP @ ?DUP WHILE
		\." checking env string " DUP DUP STRLEN TELL CR
		( a u env ca )
		SWAP >R >R
		2DUP R>
		( a u a u ca )
		DUP >R
		-ROT ENV=
		IF
			\." found var" CR
			( a u )
			NIP R> RDROP
			+ 1+ DUP STRLEN EXIT
		ELSE
			RDROP
		THEN
		R> 4+
	REPEAT
	DROP 2DROP 0 0
;

: CHDIR ( a u -- 0 (if success) | errno (if not) )
	CSTRING SYS_CHDIR SYSCALL1
	DUP 0< IF NEGATE THEN
;

(
	Here's a little secret that the man pages won't tell you: getcwd doesn't return a pointer to a cstring, it returns a length (including the null terminator). Presumably, the difference is so that glibc's wrapper function can return the address of a dynamically allocated buffer if necessary.
)

: GETCWD ( -- a u 0 (if success) | 0 0 errno (if not) )
	4096 HERE @ SYS_GETCWD SYSCALL2
	DUP 0< IF
		NEGATE 0 0 ROT
	ELSE
		HERE @ SWAP 1- 0
	THEN
;

: CWD?
	GETCWD ?DUP IF
		S" CWD?" PERROR
		2DROP
	ELSE TELL SPACE THEN
;
: CD
	WORD OVER C@ [CHAR] / = IF
		CHDIR ?DUP IF S" CD: CHDIR" PERROR THEN
		EXIT
	THEN
	( a1 u1 )
	GETCWD ?DUP IF
		S" CD: GETCWD" PERROR 2DROP
	ELSE
		2DUP + [CHAR] / SWAP !
		1+
		( a1 u1 a2 u2 )
		2DUP +
		( a1 u1 a2 u2 dest )
		-ROT >R >R
		SWAP DUP >R
		CMEMCPY
		R> R> SWAP R> +
		CHDIR ?DUP IF S" CD: CHDIR" PERROR THEN
	THEN
;

(
	FILES, CONTINUED ----------------------------------------------------------------------

	Jonesforth didn't originally define WRITE-FILE, but I thought that it should be in jonesforth.f along with the other basic file operations. So that's where it is. In this section, then, we'll get into some more advanced stuff.

	First off is stat. Linux provides three major options for stat, two which take a pathname and one which takes a file descriptor. Here, I provide words for all of them. I also provide a convenience function for getting the size of the file you just statted.
)

: STAT ( a u -- 0 | errno )
	CSTRING	HERE @
	SWAP SYS_STAT SYSCALL2
	NEGATE
;

(
	LSTAT differs from STAT in that it doesn't follow symlinks.
)
: LSTAT ( a u -- 0 | errno )
	CSTRING	HERE @
	SWAP SYS_LSTAT SYSCALL2
	NEGATE
;

: FSTAT ( fd -- 0 | errno )
	HERE @
	SWAP SYS_FSTAT SYSCALL2
	NEGATE
;

: STAT_SIZE ( -- u )
	HERE @ [ 5 CELLS ], + @
;

(
	Here's where things get really interesting. mmap is a Linux system call that maps a file into the address space of a process. It seems like a more complicated read(2) at first, but actually turns out to be pretty useful, because you don't need to find anywhere to put the file contents. This will come in handy later.
)

: PAGEALIGNED
	4095 + [ 4095 INVERT ], AND
;

: (MMAP) ( ofs fd flags prot length addr -- +addr | -errno )
	\.S CR
	SYS_MMAP SYSCALL6
;

: MMAP ( fd len flags prot -- addr 0 | x errno )
	>R >R 0 -ROT R> R>
	( 0 fd len flags prot )
	ROT 0 (MMAP)
	DUP 4095 AND
	IF \ error
		NEGATE 0 SWAP
	ELSE
		0
	THEN
;

: MMAP_FPR ( fd -- addr u 0 | x x errno ) \ maps the Full file Privately with Read enabled
	DUP FSTAT ?DUP IF
		0 SWAP EXIT
	THEN
	STAT_SIZE
	TUCK
	MAP_PRIVATE
	PROT_READ
	MMAP
	ROT SWAP
;

: MUNMAP ( addr len -- 0 | errno )
	SWAP SYS_MUNMAP SYSCALL2 NEGATE
;

(
	EVALUATE AND LOADING FILES ----------------------------------------------------------------------

	Many Forths have a word called EVALUATE, which, when given a string, evaluates it as Forth code. This is obviously pretty useful for (say) loading code files from disk. It sounds pretty simple, right? Just iterate over the string and pass each word in turn to (INTERPRET). Well.

	There are many words that call WORD or KEY, such as :, .", S", etc etc. These words will behave oddly when EVALUATEd in this naive fashion, because KEY will still return characters from standard input. The way I chose to solve this is to temporarily override KEY (see jonesforth.S). The short explanation is that you write a callback to KEYCB, which is then called instead of KEY. This callback then needs to remove itself when it's finished, by writing 0 to KEYCB.

	Here, we replace KEY with BUFKEY. BUFKEY is told where to read from by a stack of KEY_SOURCE structs, creatively named KEY_SOURCES. A KEY_SOURCE is laid out like this:

	0	address
	4	length
	8	read index
	12	next

	The first three fields should be self-explanatory. The fourth, next, is a field that describes what to do next after the buffer runs out. 0 means switch to the next source down, 1 means munmap the buffer and then switch. Yes, that's right: this implementation uses mmap to avoid having to allocate buffers for files.

	This implementation of EVALUATE isn't a true evaluate, because it only works from the prompt. This is because of how QUIT and the interpreter work. I could fix it, but I'd have to gut most of Jonesforth and I really don't feel like doing that.
)

8 CONSTANT MAX_KEY_SOURCES
4 CELLS MAX_KEY_SOURCES * ALLOT CONSTANT KEY_SOURCES
VARIABLE KEY_SOURCE_IDX -1 KEY_SOURCE_IDX ! \ always equal to # of sources - 1

: KEY_SOURCE
	KEY_SOURCE_IDX @ 16 * KEY_SOURCES +
;

: NEXT_SOURCE ( -- 1 (if source is present) | 0 (if control returned to builtin) )
	\KEY_SOURCE 16 DUMP
	KEY_SOURCE 12 + @ IF \ next != 0, munmap buffer
		\." munmapping" CR
		DUP DUP
		@ SWAP 4+ @ MUNMAP DROP
	THEN
	KEY_SOURCE_IDX DUP @ 1- DUP ROT !
	0< DUP IF
		\." no sources left" CR
		0 KEYCB !
	THEN
	NOT
;

: BUFKEY
	KEY_SOURCE DUP 4+ DUP @
	SWAP 4+ @
	= IF
		\." source empty" CR
		NEXT_SOURCE
		UNLESS	\ no sources left
			\." bufkey: no sources left" CR
			DROP (KEY) EXIT
		THEN
		\." continuing on next source" CR
		16 -
	THEN

	DUP @ OVER 8 + @ ( *source addr idx )
	TUCK + C@ ( *source idx key )
	SWAP 1+ ROT
	8 + !
;

: (EVALUATE) ( addr u next -- )
	KEY_SOURCE_IDX DUP @ DUP ( *sidx sidx sidx )
	MAX_KEY_SOURCES = IF
		0 KEYCB !
		." max evaluate depth reached!"
		QUIT
	THEN
	\." evaluate " .S CR
	1+ SWAP !

	KEY_SOURCE ( addr u next *source )
	0 OVER 8 + ! 	\ read idx
	TUCK 12 + !		\ next
	TUCK 4 + !		\ length
	!
	' BUFKEY KEYCB !
;

: EVALUATE 0 (EVALUATE) ;

: (LOAD) ( addr u -- )
	." loading " 2DUP TELL CR
	R/O OPEN-FILE ?DUP IF
		S" (LOAD): failed to open for reading" PERROR
		DROP EXIT
	THEN
	DUP
	MMAP_FPR ?DUP IF
		S" (LOAD): failed to mmap" PERROR
		2DROP EXIT
	THEN
	ROT CLOSE-FILE DROP
	1 (EVALUATE) QUIT
;

: LOAD WORD (LOAD) ;

(
	LOADLIB is used for loading files relative to a constant library path, as opposed to in the CWD. The path is found in the environment variable FORTH_LIBRARY.
)
: (LOADLIB) ( a u )
	>R >R

	S" FORTH_LIBRARY" ENVVAR
	OVER UNLESS
		2DROP ." could not find library path" CR QUIT
	THEN

	( a u )
	TUCK HERE @ SWAP CMEMCPY
	HERE @ SWAP
	2DUP + TUCK [CHAR] / SWAP ! 1+ SWAP 1+

	R> R>
	DUP >R
	( ... dest scr len )
	ROT SWAP CMEMCPY
	R> +
	(LOAD)
;


: LOADLIB
	WORD (LOADLIB)
;

(
	Jonesforth's builtin parse error reads from its own buffer, which means we need to replace it with our own. This function prints at most 40 characters leading up to the read index of the current KEY_SOURCE.
)

: BUFTEXTERR
	KEY_SOURCE
	DUP @ SWAP 8 + @
	( addr idx )
	DUP 40 > IF
		40 - + 40
	THEN
	TELL
;

LATEST @ >CFA TEXTERRCB !

: TEXTERR
	KEYCB @ IF
		TEXTERRCB @ EXECUTE
	ELSE
		TEXTERR
	THEN
;

(
	DEPENDENCY CHECKS ----------------------------------------------------------------------

	Code often depends on other code. For example, you might be writing an application that requires double precision integers, or a random number generator, or floating point support. If you try to LOAD a file without first LOADing what it depends on, you'll get a stream of parse errors and maybe break something.

	LOADED does two things: first, it prints "OK". More importantly, it creates a marker word with the same name as the file. REQUIRE can check for this word at the top of another file, and if it doesn't find it, quit back to the regular reading-from-stdin prompt.

	INCLUDE and INCLUDELIB also check for these marker words, but instead of simply erroring, search for and load the named file in the CWD and library respectively. It's not a perfect solution (no namespacing, requires you to specify the file name twice) but it's good enough.

	Basic Principle!
)

: LOADED
	WORD
	\2DUP ." loaded " TELL CR
	\UNUSED DUP . ." cells (" CELLS . ." bytes) remaining" CR
	." OK" CR
	CREATE DOCOL , ' EXIT ,
;

: REQUIRE
	WORD 2DUP FIND
	UNLESS
		TELL SPACE ." required, not found" CR
		-1 KEY_SOURCE_IDX !
		0 KEYCB ! QUIT
	THEN
	2DROP
;

: INCLUDE
	WORD 2DUP FIND
	UNLESS (LOAD) ELSE 2DROP THEN
;
: INCLUDELIB
	WORD 2DUP FIND
	UNLESS (LOADLIB) ELSE 2DROP THEN
;

: LOAD_ARG
	ARGC 1 > IF
		1 ARGV (LOAD)
	THEN
;

(
	This is a hack to allow running Forth programs as posix executable text scripts. I'm going to assume here you know how those work, so I won't explain them. In short, start a Forth program with "#!/bin/env jamforth" on a line by itself, and it will work fine. Of course, replace "jamforth" with whatever you have this whole mess called.
)
: #!/bin/env
	BEGIN KEY '\n' = UNTIL
;

DEC
LOADED aforth.f

LOAD_ARG
