# $Id: Makefile,v 1.9 2007-10-22 18:53:12 rich Exp $

#BUILD_ID_NONE := -Wl,--build-id=none 
BUILD_ID_NONE := 

SHELL	:= /bin/bash

all:	jonesforth

jonesforth: jonesforth.S
	gcc -m32 -nostdlib -static $(BUILD_ID_NONE) -I /usr/include/x86_64-linux-gnu -o $@ $<

run:
	cat jonesforth.f $(PROG) - | ./jonesforth

clean:
	rm -f jonesforth perf_dupdrop *~ core .test_*
