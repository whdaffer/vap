.SUFFIXES: 
.SUFFIXES: .h .c .o .so 
.PHONEY: clean

CFLAGS=-g
LDFLAGS=
CC=gcc
SRC=objanl.c test_objanl.c
OBJ=objanl.o test_objanl.o

.c.o:
	gcc -c $(CFLAGS) $<

test_objanl: $(OBJ)
	gcc $(CFLAGS) -o test_objanl $(OBJ)	

clean: 
	rm -f *.o 

real_clean:
	make clean
	rm test_objanl