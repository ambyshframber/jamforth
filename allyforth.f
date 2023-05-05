: COUNT ( addr len -- addr len first )
    ( get the first character of a string and advance the string pointer )
    OVER C@
    ROT 1+
    ROT 1-
    ROT
;
