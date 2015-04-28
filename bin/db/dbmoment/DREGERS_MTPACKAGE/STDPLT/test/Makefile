CC=gcc
CFLAGS= -g -arch x86_64 -Wno-return-type -Wno-format -Wno-implicit
#GETPAR= /home/u1/dreger/Sources/libget.a
GETPAR= ../../libget.a
PLOTLIB= ../../libstd.a
#PLOTLIB= ../plotsubs/stdplt
#PLOTLIB= -lstd

all:	box mtest symbols test fan text

box:	box.c
	${CC} ${CFLAGS} -o box box.c ${PLOTLIB} ${GETPAR} -lm

mtest:	mtest.c
	${CC} ${CFLAGS} -o mtest mtest.c ${PLOTLIB} ${GETPAR} -lm
symbols:	symbols.c
	${CC} ${CFLAGS} -o symbols symbols.c ${PLOTLIB} ${GETPAR} -lm

csymbols:	csymbols.c
	${CC} ${CFLAGS} -o csymbols csymbols.c ${PLOTLIB} ${GETPAR} -lm

test:	test.c
	${CC} ${CFLAGS} -o test test.c ${PLOTLIB} ${GETPAR} -lm

fan:	fan.c
	${CC} ${CFLAGS} -o fan fan.c ${PLOTLIB} ${GETPAR} -lm

text:	text.c
	${CC} ${CFLAGS} -o text text.c ${PLOTLIB} ${GETPAR} -lm

cleanup:
	-rm -f test text box symbols csymbols fan
