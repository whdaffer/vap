
# Change to reflect your needs.
DELIVERY_DIR=/users/whdaffer/delivered/idl

RSIINC=${IDL_DIR}/external/include
CC=gcc
#LD=ld
LD=g77
F77=g77
#CCFLAGS=-fPIC -c -D_REENTRANT  
#F77FLAGS=
# uncomment to get some debugging info from the C code.
CCFLAGS=-fPIC -c -D_REENTRANT  -DDEBUG 
F77FLAGS=-DDEBUG

INCFLAGS=-I ${RSIINC} -I ./

#echo "Done";
BINDIR=$(IDL_DIR)/bin/bin.linux.x86
IDLLIBS=-L$(BINDIR) -lidl -Wl,-rpath $(BINDIR)
LDFLAGS = -shared -L$(BINDIR) -lidl -Wl,-rpath,. -Wl,-rpath $(BINDIR)\
	-lXp -L/usr/X11R6/lib -lXpm -lXmu -lXext -lXt -lSM -lICE -lX11 -ldl \
	-ltermcap -lrt -lm -lpthread -lgcc_s
LIBS=-lc -lm 


# LDFLAGS= -L$(BINDIR) -lidl -Wl,-rpath,. -Wl,-rpath $(BINDIR)\
# 	/usr/X11R6/lib/libXm.a \
# 	-lXp -L/usr/X11R6/lib -lXpm -lXext -lXt -lSM -lICE -lX11 -ldl \
# 	-ltermcap -lrt -lm -lpthread

#	EquivLat.c \

C_SRCFILES=succor.c

F_SRCFILES=objanl2.f


OBJFILES=succor.o objanl2.o



default: succor.so

clean: 
	rm -f *.o *.so

.c.o:${C_SRCFILES}
	${CC} ${CCFLAGS} ${INCFLAGS}  -c $<

.f.o:${F_SRCFILES}
	$(F77) $(F77FLAGS) $(INCFLAGS) -c $<


succor.so: ${OBJFILES}
	${LD}  ${LDFLAGS} -o succor.so \
	${OBJFILES} \
	${IDLLIBS} \
	${LIBS}



install: succor.so
	/bin/cp -f succor.so succor.dlm ${DELIVERY_DIR}


