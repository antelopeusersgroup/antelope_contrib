#STDLIB = ../

CC=gcc
SOURCE = frame.c scale.c table.c err.c plot.c dot.c \
	text.c  polygon.c output.c erase.c symbol.c \
	puth.c fortran.c
FILES=	${SOURCE:%.c=%.o}

STDLIB = ../..

INC1=/usr/include/malloc
INCDIRS=-I${INC1}
SYM = 	symbolname.h
HEADERS = ../h/igl.h stdplt.h
CFLAGS= ${INCDIRS} -O  -Wno-return-type -arch x86_64 -Wno-implicit -Wno-format

stdplt:	$(FILES) $(SYM)
	ar cr stdplt $(FILES)
	ranlib stdplt 
	\mv stdplt ../../libstd.a

symbol.o: ${SYM}

${SYM}:	../symbol/symboldata
	(cd ../symbol; make )

testload:
	${CC} -g dummymain.c stdplt -lm
	@echo "execute test"
	a.out > /dev/null


$(FILES): $(HEADERS)

list:
	imprint  makefile sublist ${HEADERS} $(SOURCE)

install:
	install -c -m 644 stdplt ${STDLIB}/libstd.a
	ranlib ${STDLIB}/libstd.a

cleanup:
	-rm -f $(FILES) stdplt
