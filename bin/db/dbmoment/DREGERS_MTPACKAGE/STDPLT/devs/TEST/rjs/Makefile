#
# Makefile for Suntools
#
NAME=igltool
DIR=../sun
HEADERS=../../h/igl.h devpar.h
SOURCE= open_close.c vector.c boxfill.c xpolygon.c color.c initcolortable.c \
	getspcpar.c text.c readcom.c
LOC_OBJS= globalvar.o open_close.o vector.o boxfill.o xpolygon.o color.o \
	initcolortable.o getspcpar.o text.o readcom.o
COM_OBJS=../com/main.o ../com/line_point.o ../com/symbol.o\
	../com/err.o ../com/getstdpar.o \
	../com/label.o ../com/dot.o ../com/geth.o
DUM_OBJS= ../dum/raster.o ../dum/pattern.o \
	../dum/newplot.o ../dum/sendplot.o
SYM_OBJS=../../symboldata/getsymdata.o
CFLAGS=-g

${NAME}: ${LOC_OBJS}  ${COM_OBJS} ${DUM_OBJS} ${SYM_OBJS}
	cc ${CFLAGS} -Bstatic  -o ${NAME} ${LOC_OBJS} $(SCAN_OBJS) ${COM_OBJS} \
	${DUM_OBJS} ${SYM_OBJS} /home/u1/dreger/Sources/libget.a -lsuntool -lsunwindow -lpixrect -lm

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
	imprint makefile devpar.h $(SOURCE)

install:
	install -c ${NAME} ${DEVDIR}

xinstall:
	install -c ${NAME} ${DEVDIR}/x${NAME}

cleanup:
	-rm -f $(LOC_OBJS) $(NAME)
