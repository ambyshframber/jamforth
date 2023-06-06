: LOOKUPXT WORD FIND >CFA ;

: COUNT ( addr len -- addr len first )
    ( get the first character of a string and advance the string pointer )
    OVER C@
    ROT 1+
    ROT 1-
    ROT
;

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

(
    IMMEDIATE IF

    The eponymous Jones has left us some homework: "Making [the control structures] work in immediate mode is left as an exercise for the reader."

    The solution I thought of is to compile a temporary word, EXECUTE it, then FORGET it. This requires modifications to IF and THEN, but not ELSE. The main difference is an additional value left on the stack, underneath the address of the 0BRANCH word that gets compiled. This additional value tells THEN whether to compile as normal, or end compilation and execute the word. Conveniently, we can use the xt returned by :NONAME as a flag. 
)

: IF IMMEDIATE
    STATE @ IF \ compiling
        0 \ leave flag for THEN
	    ' 0BRANCH , \ compile 0BRANCH
	    HERE @ \ save location of the offset on the stack
	    0 , \ compile a dummy offset
    ELSE \ immediate
        :NONAME \ start compiling an anonymous word
        ' 0BRANCH ,
        HERE @
        0 ,
    THEN
;

: THEN IMMEDIATE
	DUP
	HERE @ SWAP - \ calculate the offset from the address saved on the stack
	SWAP ! \ store the offset in the back-filled location
    
    ?DUP IF \ check flag/xt left by IF
        ' EXIT , \ finish off word
        EXECUTE \ execute it
        
        LATEST @ DUP @ LATEST !
        HERE ! \ make sure it doesn't leak

        [COMPILE] [
    THEN
;
