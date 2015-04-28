#
# Makefile for Suntools driver.
#
NAME=igltool
DIR=../sun
HEADERS=../../h/igl.h devpar.h
SOURCE= open_close.c vector.c boxfill.c polygon.c color.c initcolortable.c \
	getspcpar.c rastertext.c sendplot.c get_fb_dim.c
LOC_OBJS= globalvar.o open_close.o vector.o boxfill.o polygon.o color.o \
	initcolortable.o getspcpar.o rastertext.o sendplot.o get_fb_dim.o
COM_OBJS=../com/main.o ../com/readcom.o ../com/line_point.o ../com/symbol.o\
	../com/err.o ../com/text.o  ../com/getstdpar.o \
	../com/label.o ../com/dot.o ../com/geth.o
DUM_OBJS= ../dum/raster.o ../dum/pattern.o \
	../dum/newplot.o
SYM_OBJS=../../symboldata/getsymdata.o
CFLAGS=	-O 
DEVDIR=	/usr/contrib/bin/stdplt
PRINT=	lpr -p

${NAME}: ${LOC_OBJS}  ${COM_OBJS} ${DUM_OBJS} ${SYM_OBJS}
	cc ${CFLAGS} -o ${NAME} ${LOC_OBJS} $(SCAN_OBJS) ${COM_OBJS} \
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
	${PRINT} makefile devpar.h $(SOURCE)

install:
	install -c ${NAME} ${DEVDIR}

xinstall:
	install -c ${NAME} ${DEVDIR}/x${NAME}

cleanup:
	-rm -f $(LOC_OBJS) $(NAME) a.out core
