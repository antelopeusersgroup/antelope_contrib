#
# Makefile for Imagen
#
NAME=ima
DIR=../ima
HEADERS=../../h/igl.h devpar.h
SOURCE= sendplot.c getspcpar.c bitfix.c
LOC_OBJS=globalvar.o sendplot.o getspcpar.o bitfix.o patch.o
SCAN_OBJS=../scan/bits.o ../scan/zap4.o ../scan/newplot.o ../scan/linefill.o \
	../scan/raster.o ../scan/vector.o ../scan/boxfill.o
COM_OBJS=../com/main.o ../com/readcom.o ../com/line_point.o ../com/symbol.o \
	../com/err.o ../com/text.o  ../com/getstdpar.o ../com/polygon.o \
	../com/label.o ../com/rastertext.o ../com/geth.o ../com/dot.o
DUM_OBJS=../dum/opendev.o ../dum/closedev.o ../dum/color.o \
	../dum/erase.o	../dum/pattern.o ../dum/initcolortable.o
SYM_OBJS=../../symboldata/getsymdata.o
CFLAGS= -O -fsingle
DEVDIR=	/usr/contrib/bin/stdplt
PRINT=	lpr -p

${NAME}: ${LOC_OBJS} $(SCAN_OBJS) ${COM_OBJS} ${DUM_OBJS} ${SYM_OBJS}
	cc ${CFLAGS} -o ${NAME} ${LOC_OBJS} $(SCAN_OBJS) ${COM_OBJS} \
		${DUM_OBJS} ${SYM_OBJS} /home/u1/dreger/Sources/libget.a -lm

globalvar.o: devpar.h ../com/globalvar.c
	cc -c ${CFLAGS} -I${DIR} ../com/globalvar.c

patch.o: devpar.h ../scan/patch.c
	cc -c ${CFLAGS} -I${DIR} ../scan/patch.c

${SYM_OBJS}:
	(cd ../../symboldata; make)

${SCAN_OBJS}:
	(cd ../scan; make )

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
