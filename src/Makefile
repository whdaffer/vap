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
# Revision 1.6  2003/01/30 19:11:05  vapdev
# Ooopsss. Need ainf too!
#
# Revision 1.5  2003/01/30 19:04:14  vapdev
# Modified how grid_goes is built
#
# Revision 1.4  2003/01/28 19:05:55  vapdev
# Added schop
#
# Revision 1.3  2002/08/09 23:47:17  vapdev
# changed '(' to '{'
#
# Revision 1.2  2002/07/10 15:41:45  vapdev
#
# Simplified the Makefile
#
#
#
#

#.SUFFIXES : .o .c .f


OBJ = grid_goes.o navig_goes.o pair_goes.o doy2date.o qchop.o schop.o ainf.c
SRC = grid_goes.c doy2date.c navig_goes.f pair_goes.f qchop.c qchop.c ainf.o

GRIDOBJ = grid_goes.o navig_goes.o pair_goes.o doy2date.o 
GRIDSRC = grid_goes.c doy2date.c navig_goes.f pair_goes.f 

CFLAGS = -g -o32 $(DEBUG)
CC     = cc 
LD     = cc -o32 -o 
F77    = f77 -g -o32 -c


INSTALLDIR = ../bin

#LIBS   =  -lF77 -lm  -lc
LIBS   =  -lm  -lc
FLIBS = -lftn 

.c.o: 
	${CC} ${CFLAGS} -c $<

.f.o: 
	${F77} $<

default: grid_goes qchop schop ainf


clean:
	-/sbin/rm *.o

distclean:
	make clean
	-/sbin/rm grid_goes qchop schop ainf


grid_goes: ${GRIDOBJ} 
	${CC} -o grid_goes ${GRIDSRC} ${FLIBS} ${LIBS}
	echo Done!

grid_goes.o: grid_goes.c
	${CC} ${CFLAGS} -c grid_goes.c

#doy2date.o: doy2date.c
#	${CC} ${CFLAGS} doy2date.c

navig_goes.o: navig_goes.f
	${F77} navig_goes.f # may use -d_lines  to get aditional output

pair_goes.o: pair_goes.f
	${F77} pair_goes.f # may use -d_lines  to get aditional output

qchop: qchop.o
	${CC} ${CFLAGS} -o qchop qchop.c ${LIBS}

schop: schop.o
	${CC} ${CFLAGS}-o schop schop.c ${LIBS}

ainf: ainf.o
	${CC} ${CFLAGS} -o ainf ainf.c ${LIBS}

install: grid_goes qchop schop ainf
	-/bin/cp -f grid_goes ${INSTALLDIR}
	-/bin/cp -f qchop ${INSTALLDIR}
	-/bin/cp -f schop ${INSTALLDIR}
	-/bin/cp -f ainf ${INSTALLDIR}





