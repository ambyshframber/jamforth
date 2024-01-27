: [CHAR] IMMEDIATE CHAR [COMPILE] LITERAL ;

: LOOKUPXT WORD FIND >CFA ;

: COUNT ( addr len -- addr len first )
    ( get the first character of a string and advance the string pointer )
    OVER C@
    ROT 1+
    ROT 1-
    ROT
;
: COUNT4 ( addr len -- addr len first )
    \ like COUNT but with cell-sized data
    OVER @ ROT 4+ ROT 1- ROT
;

: BINARY 2 BASE ! ;
: BIN BINARY ;

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

\ simple non-secure random number generator
\ because it's so simple, it's also shit
: RANDOMC RDTSC DROP DUP 4 >> SWAP 12 >> XOR 255 AND ;

\ less simple and less shit rng based on an lfsr and the timestamp counter
VARIABLE LFSR
RANDOMC RANDOMC 8 << OR 1 OR LFSR ! \ 1 or makes sure it doesn't get filled with all zeros
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

: MAX ( a b -- max )
    2DUP > IFELSE DROP NIP
;
: MIN ( a b -- min )
    2DUP < IFELSE DROP NIP
;

(
    IMMEDIATE IF

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
    BETTER DECOMPILATION

    Jonesforth's SEE segfaults if it encounters a word it doesn't understand, which isn't great. With certain gnarlier dictionary arrangements (not really that gnarly - a simple ALLOT or VARIABLE can break SEE for the previous word) it's completely useless. So, I've made a slightly less failure-prone but also less ergonomic decompiler.

    DC-CELL takes a 32-bit value and prints its signed decimal and unsigned hex values, its value when interpreted as a 4-character ASCII string (replacing non-printing characters with "."), and its name (or "?").
)

: DC-CELL
    BASE @ SWAP

    DUP 2DUP
    HEX 8 U.R
    DECIMAL 12 .R
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

    CFA> ?DUP IF ID. ELSE [CHAR] ? EMIT THEN

    CR

    BASE !
;

: DC-CELLS ( addr len ) \ len is number of cells, not number of bytes
    BEGIN ?DUP WHILE
        COUNT4 DC-CELL
    REPEAT
;

(
    MODULES

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

    0   backpointer to previous module OR null
    4   word to be selected when enabled
    8   word to be selected when disabled
    12  word to have its backpointer changed ("guard word")
    16  namelen + flags (1 byte)
    17  name


    namelen + flags
    bits [5,0] = length
    bit 6 = module enabled (1 when in, 0 when out)
)
