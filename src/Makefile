# VAP src Makefile
#
# $Id$
#
#
# $Id$
#
# Change Log:
#
# $Log$
# Revision 1.2  2002/07/10 15:41:45  vapdev
#
# Simplified the Makefile
#
#
#
#

#.SUFFIXES : .o .c .f


OBJ = grid_goes.o navig_goes.o pair_goes.o doy2date.o
SRC = grid_goes.c doy2date.c navig_goes.f pair_goes.f

CFLAGS = -c -g -o32 $(DEBUG)
CC     = cc $(CFLAGS)
LD     = cc -o32 -o 
F77    = f77 -g -o32 -c

#LIBS   =  -lF77 -lm  -lc
LIBS   =  -lftn -lm  -lc

.c.o: 
	${CC} ${CFLAGS} $<

.f.o: 
	${F77} $<

default: grid_goes
	make grid_goes

clean:
	-/sbin/rm *.o


grid_goes: ${OBJ}
	${LD} grid_goes ${OBJ} ${LIBS}
	echo Done!

grid_goes.o: grid_goes.c
	${CC} ${CFLAGS} grid_goes.c

doy2date.o: doy2date.c
	${CC} ${CFLAGS} doy2date.c

navig_goes.o: navig_goes.f
	${F77} navig_goes.f # may use -d_lines  to get aditional output

pair_goes.o: pair_goes.f
	${F77} pair_goes.f # may use -d_lines  to get aditional output

install:
	-/bin/cp -f grid_goes ${INSTALLDIR}





