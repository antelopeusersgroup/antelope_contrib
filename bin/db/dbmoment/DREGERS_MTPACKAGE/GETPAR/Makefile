LIB = libget.a

include $(ANTELOPEMAKE)

CFLAGS=-arch x86_64 -Wno-implicit -Wno-return-type -c -O -DENVIRONMENT

SOURCE = getpar.c getarg.c
CFILES = getpar.o getarg.o

libget.a:
	gcc $(CFLAGS) $(SOURCE)
	ar r get $(CFILES)
	ranlib get
	ar tv get
	\mv get ../libget.a
