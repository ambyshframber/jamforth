# $Id: Makefile,v 1.9 2007-10-22 18:53:12 rich Exp $

#BUILD_ID_NONE := -Wl,--build-id=none 
BUILD_ID_NONE := 

SHELL	:= /bin/bash

all:	jonesforth

# --verbose -Wl,--verbose

jonesforth: jonesforth.S qoi.c
	gcc -m32 -nostdlib -static $(BUILD_ID_NONE) -I /usr/include/x86_64-linux-gnu -o $@ jonesforth.S qoi.c

run: all
	cat jonesforth.f allyforth.f $(PROG) - | ./jonesforth

clean:
	rm -f jonesforth perf_dupdrop *~ core .test_*
