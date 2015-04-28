#
# Makefile for zeta
#
NAME=zeta
DIR=../zeta
MANDIR = /usr/man
MANEXT = l
HEADERS=../../h/igl.h devpar.h
SOURCE= color.c erase.c getspcpar.c newplot.c line_point.c \
	open_close.c poly.c sendplot.c speed.c vector.c
LOC_OBJS= globalvar.o color.o erase.o getspcpar.o line_point.o newplot.o \
	open_close.o poly.o sendplot.o speed.o vector.o
COM_OBJS=../com/main.o ../com/readcom.o ../com/symbol.o\
	../com/err.o ../com/text.o  ../com/getstdpar.o ../com/xpolygon.o \
	../com/label.o ../com/dot.o ../com/geth.o 
DUM_OBJS= ../dum/raster.o ../dum/pattern.o ../dum/rastext.o \
	../dum/initcolortable.o
SYM_OBJS=../../symboldata/getsymdata.o
CFLAGS=	-O -fsingle
DEVDIR=	/usr/contrib/bin/stdplt
PRINT=	lpr -p

${NAME}: ${LOC_OBJS}  ${COM_OBJS} ${DUM_OBJS} ${SYM_OBJS}
	cc ${CFLAGS} -Bstatic  -o ${NAME} ${LOC_OBJS} $(SCAN_OBJS) ${COM_OBJS} \
		${DUM_OBJS} ${SYM_OBJS} /home/u1/dreger/Sources/libget.a -lm

globalvar.o: devpar.h ../com/globalvar.c
	cc -c ${CFLAGS} -I${DIR} ../com/globalvar.c


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

man:	
	install -c ${NAME}.man ${MANDIR}/man${MANEXT}/${NAME}.${MANEXT}

xinstall:
	install -c ${NAME} ${DEVDIR}/x${NAME}

cleanup:
	-rm -f $(LOC_OBJS) $(NAME) a.out core
