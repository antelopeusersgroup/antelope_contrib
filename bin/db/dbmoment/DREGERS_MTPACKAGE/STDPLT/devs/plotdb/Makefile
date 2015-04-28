#
# Makefile for debugging driver
#
NAME= plotdb
DIR= ../plotdb
HEADERS= ../../h/igl.h devpar.h
SOURCE=	  readcom.c getspcpar.c
LOC_OBJS= readcom.o getspcpar.o globalvar.o
COM_OBJS= ../com/main.o  ../com/err.o ../com/geth.o
DUM_OBJS= ../dum/open_close.o ../dum/getstdpar.o ../dum/raster.o \
	  ../dum/newplot.o ../dum/initcolortable.o ../dum/label.o
CFLAGS=	-g
DEVDIR=	/usr/contrib/bin/stdplt
PRINT=	lpr -p

${NAME}: ${LOC_OBJS} ${COM_OBJS} ${DUM_OBJS}
	cc ${CFLAGS} -o ${NAME} ${LOC_OBJS} ${COM_OBJS} ${DUM_OBJS} ../../../libget.a -lm

globalvar.o: devpar.h ../com/globalvar.c
	cc -c ${CFLAGS} -I${DIR} ../com/globalvar.c

${COM_OBJS}:
	(cd ../com; make )

${DUM_OBJS}:
	(cd ../dum; make dumsubs )

$(LOC_OBJS): $(HEADERS)

list:
	${PRINT} makefile devpar.h $(SOURCE)

install:
	install -c ${NAME} ${DEVDIR}

xinstall:
	install -c ${NAME} ${DEVDIR}/x${NAME}

cleanup:
	-rm -f $(LOC_OBJS) $(NAME) a.out core
