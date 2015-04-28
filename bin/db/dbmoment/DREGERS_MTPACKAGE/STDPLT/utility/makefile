CFLAGS= -g

all: mkascii mkbin

mkascii: trans.c
	cc $(CFLAGS) -DMKASCII -o mkascii trans.c

mkbin: trans.c
	cc $(CFLAGS) -DMKBIN -o mkbin trans.c
