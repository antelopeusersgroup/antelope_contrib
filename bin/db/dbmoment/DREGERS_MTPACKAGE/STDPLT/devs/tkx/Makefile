#
# Makefile for Tektronix
#
NAME=tkx
DIR=../tkx
HEADERS=../../h/igl.h devpar.h
SOURCE= open_close.c vector.c newplot.c fill.c sendplot.c
LOC_OBJS= globalvar.o open_close.o vector.o newplot.o fill.o sendplot.o
COM_OBJS=../com/main.o ../com/readcom.o ../com/line_point.o ../com/symbol.o\
	../com/err.o ../com/text.o  ../com/getstdpar.o ../com/xpolygon.o \
	../com/label.o ../com/dot.o ../com/geth.o
DUM_OBJS= ../dum/raster.o ../dum/getspcpar.o  ../dum/color.o \
	../dum/rastext.o ../dum/pattern.o ../dum/initcolortable.o
SYM_OBJS=../../symboldata/getsymdata.o
#CFLAGS=	-O -fsingle-precision-constant
CFLAGS=	-c -arch x86_64 -Wno-implicit -Wno-return-type -Wno-format
DEVDIR= ../..
PRINT=	lpr -p
CC=gcc

${NAME}: ${LOC_OBJS}  ${COM_OBJS} ${DUM_OBJS} ${SYM_OBJS} libget
	${CC} ${CFLAGS} ${SOURCE}

load:
	/usr/bin/ld -o ${NAME} ${LOC_OBJS} ${COM_OBJS} ${DUM_OBJS} ${SYM_OBJS} ../../../libget.a -lm

globalvar.o: devpar.h ../com/globalvar.c
	${CC} -c ${CFLAGS} -I${DIR} ../com/globalvar.c


${SYM_OBJS}:
	(cd ../../symboldata; make)

${COM_OBJS}:
	(cd ../com; make )

${DUM_OBJS}:
	(cd ../dum; make )

$(LOC_OBJS): $(HEADERS)

list:
	${PRINT} makefile devpar.h $(SOURCE)

install:
	install -c ${NAME} ${DEVDIR}

xinstall:
	install -c ${NAME} ${DEVDIR}/x${NAME}

cleanup:
	-rm -f $(LOC_OBJS) $(NAME) a.out core

libget:
	(cd ../../../GETPAR; make get)
