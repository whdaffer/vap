#
# Makefile for the various linkimage routines we use.
#
#
# $Log$
# Revision 1.1  1999/04/09 23:12:03  vapuser
# Initial revision
#
#
#
# $Id$
#
CC = cc
F77 = f77
LD = ld 
CFLAGS = -KPIC -mips2
F77_FLAGS = -KPIC -mips2 -d_lines
F77LIBs = -lftn
LDFLAGS = -shared -mips2
CLIBS = -lc -lm
RSIINC=/usr/local/rsi/idl_5.1/external


.c.o:
	cc -c $(CFLAGS) $<


SRCS = \
	isanumber.c \
	land_mask.c \
	succor.c \
	wvcsel.c \
	kprimes.c

SO_OBJS = \
	isanumber.so \
	land_mask.so \
	succor.so \
	wvcsel.so \
	kprimes.so

default: land_mask.so succor.so kprimes.so

#isanumber.so: isanumber.o isanumber.c \
#	$(RSIINC)/export.h
#	echo Makeing isnumber.so
#	$(CC) $(CFLAGS) -c isanumber.c
#	$(LD) $(LDFLAGS) -o isanumber.so isanumber.o  $(CLIBS)

 
kprimes.so: kprimes.o kprimes.c \
	$(RSIINC)/export.h
	echo Makeing kprimes.so
	$(CC) $(CFLAGS) -c kprimes.c
	$(LD) $(LDFLAGS) -o kprimes.so kprimes.o  $(CLIBS)

#setfunctions.so: setfunctions.c setfunctions.o \
#	$(RSIINC)/export.h
#	echo Makeing setfunctions.so
#	$(CC)  $(CFLAGS) -c setfunctions.c
#	$(LD)  $(LDFLAGS) -o setfunctions.so setfunctions.o $(CLIBS)

succor.so: succor.o subobj.o \
	$(RSIINC)/export.h
	echo Makeing succor.so
	$(CC) $(CFLAGS) -c succor.c
	$(F77) $(F77_FLAGS) -c subobj.f
	$(LD) $(LDFLAGS) -o succor.so succor.o subobj.o $(CLIBS) $(F77LIBS)


land_mask.so: land_mask.o lndmsk.o \
	$(RSIINC)/export.h
	echo Makeing land_mask.so
	$(CC) $(CFLAGS) -c land_mask.c  lndmsk.c
	$(LD) $(LDFLAGS) -o land_mask.so land_mask.o lndmsk.o $(CLIBS)


#wvcsel.so: wvcsel.o \
#	$(RSIINC)/export.h
#	echo Makeing wvcsel.so
#	$(CC) $(CFLAGS) -c wvcsel.c
#	$(LD) $(LDFLAGS) -o wvcsel.so  wvcsel.o $(CLIBS)





