# $Id$
#
#
#
#
AREA_FILE_TOP=\"/disk4/vap/goes/\"

OBJ = grid_goes.o navig_goes.o pair_goes.o doy2date.o
SRC = grid_goes.c doy2date.c navig_goes.f pair_goes.f

DFLAGS = -DAREA_FILE_TOP=$(AREA_FILE_TOP)
CFLAGS = -c -g $(DEBUG)
CC     = cc $(CFLAGS)
LD     = cc -o 
F77    = f77 -g -c

LIBS   = -lF77  -lI77 -lisam -lm  -lU77 -lc

.c.o:
	$(CC) $(DFLAGS) $<

.f.o: 
	$(F77) $<

default: grid_goes
	make grid_goes
	make install

clean:
	-/sbin/rm *.o


grid_goes: $(OBJ) $(SRC)
	$(LD) grid_goes $(OBJ) $(LIBS)
	echo Done!

grid_goes.o: grid_goes.c
	$(CC) $(DFLAGS) grid_goes.c

doy2date.o: doy2date.c
	$(CC) $(DFLAGS) doy2date.c

navig_goes.o: navig_goes.f
	$(F77) navig_goes.f # may use -d_lines  to get aditional output

pair_goes.o: pair_goes.f
	$(F77) pair_goes.f # may use -d_lines  to get aditional output

install:
	-/bin/cp -f grid_goes /usr/people/vapuser/bin/

