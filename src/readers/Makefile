# $Id$
#
#
#
#
default: example_reader
example_reader: example_reader.c qread_rmgdr.o
	cc -g -o example_reader example_reader.c qread_rmgdr.o -lc -lm
qread_rmgdr.o: qread_rmgdr.c qread_rmgdr.h
	cc -g -c qread_rmgdr.c
qread_rmgdr.h:
