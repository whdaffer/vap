#
# Makefile for the various linkimage routines we use.
#
#
# $Log$
# Revision 1.4  1999/10/05 17:38:12  vapuser
# Added some debug targets.
#
# Revision 1.3  1999/06/17 19:15:23  vapuser
# Put isanumber back in
#
# Revision 1.2  1999/04/09 23:28:43  vapuser
# Added kprimes.so to default list
#
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
F77_FLAGS = -KPIC -mips2 
F77LIBs = -lftn
LDFLAGS = -shared -mips2
CLIBS = -lc -lm
RSIINC=/usr/local/rsi/idl_5.1/external


.c.o:
	$(CC) $(CFLAGS) $<

.f.o:
	$(F77) $(F77_FLAGS) $<


C_SRCS = \
	isanumber.c \
	land_mask.c \
	succor.c \
#	succor2.c \
	wvcsel.c \
	kprimes.c

F_SRCS = objanl.f subobj.f

SO_OBJS = \
	isanumber.so \
	land_mask.so \
	succor.so \
#	succor2.so \
	wvcsel.so \
	kprimes.so

default: land_mask.so succor.so kprimes.so

isanumber.so: isanumber.o isanumber.c \
	$(RSIINC)/export.h
	echo Makeing isnumber.so
	$(CC) $(CFLAGS) -c isanumber.c
	$(LD) $(LDFLAGS) -o isanumber.so isanumber.o  $(CLIBS)

 
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

#succor.so: succor.o subobj.o \
#	$(RSIINC)/export.h
#	echo Makeing succor.so
#	$(CC) $(CFLAGS) -c succor.c
#	$(F77) $(F77_FLAGS) -c subobj.f
#	$(LD) $(LDFLAGS) -o succor.so succor.o subobj.o $(CLIBS) $(F77LIBS)

succor.so: succor.o objanl.o succor.c objanl.f \
	$(RSIINC)/export.h
	echo Making succor.so
	$(CC) $(CFLAGS) -c succor.c
	$(F77) $(F77_FLAGS) -c objanl.f  
	$(LD) $(LDFLAGS) -o succor.so succor.o objanl.o $(CLIBS) $(F77LIBS)

succor-debug.so: succor-debug.o objanl-debug.o objanl.f succor.c
	echo Making succor-debug.so 
#	$(CC) $(CFLAGS) -DDEBUG -c -o succor-debug.o succor.c
#	$(F77) $(F77_FLAGS) -d_lines -c -o objanl-debug.o objanl.f  
	$(LD) $(LDFLAGS) -o succor-debug.so succor-debug.o \
	objanl-debug.o $(CLIBS) $(F77LIBS)

succor.o: succor.c
	$(CC) $(CFLAGS) -c -o succor.o succor.c

succor-debug.o: succor.c
	$(CC) $(CFLAGS) -DDEBUG -c -o succor-debug.o succor.c

objanl-debug.o: objanl.f
	$(F77) $(F77_FLAGS) -d_lines -c -o objanl-debug.o objanl.f  

objanl-test.o: objanl-test.f
	$(F77) $(F77_FLAGS) -c -o objanl-test.o objanl-test.f  

new-succor.so: succor.o objanl-test.o \
	$(RSIINC)/export.h
	echo Making new-succor.so
	$(LD) $(LDFLAGS) -o new-succor.so succor.o objanl-test.o $(CLIBS) $(F77LIBS)


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

test_objanl: test_objanl.o objanl.o
	echo Making test_objanl
	cc -g -c -mips2 test_objanl.c
	f77 -g -mips2 -d_lines -o test_objanl test_objanl.o objanl.f -lc -lm -lftn


test_objanl-debug: test_objanl.o objanl.o
	echo Making test_objanl DEBUG
	cc -g -c -mips2 -DDEBUG test_objanl.c
	f77 -g -mips2 -d_lines -o test_objanl test_objanl.o objanl.f -lc -lm -lftn


