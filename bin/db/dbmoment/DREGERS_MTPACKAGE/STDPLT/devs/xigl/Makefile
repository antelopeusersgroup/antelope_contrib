#
# Makefile for Suntools driver.
#
NAME=xigl
DIR=../xigl
HEADERS=../../h/igl.h devpar.h xigl.h
SOURCE= main.c open_close.c vector.c boxfill.c polygon.c color.c \
	initcolortable.c 	getspcpar.c sendplot.c \
	get_fb_dim.c
LOC_OBJS= $(SOURCE:%.c=%.o)

COM_OBJS= ../com/readcom.o ../com/line_point.o ../com/symbol.o\
	../com/err.o ../com/text.o  ../com/getstdpar.o \
	../com/label.o ../com/dot.o ../com/geth.o
DUM_OBJS= ../dum/raster.o ../dum/pattern.o \
	../dum/newplot.o ../dum/rastext.o

SYM_OBJS=../../symboldata/getsymdata.o
LIBDIR = $(OPENWINHOME)/lib
INCDIR = $(OPENWINHOME)/include
CFLAGS=	-g -I$(INCDIR)
DEVDIR=	/usr/contrib/bin/stdplt
PRINT=	lpr -p

${NAME}: ${LOC_OBJS} globalvar.o  ${COM_OBJS} ${DUM_OBJS} ${SYM_OBJS}
	cc ${CFLAGS} -o ${NAME} ${LOC_OBJS} globalvar.o  $(SCAN_OBJS) \
		${COM_OBJS} \
		${DUM_OBJS} ${SYM_OBJS} \
		-L$(LIBDIR) /home/u1/dreger/Sources/libget.a -lxview -lolgx -lX11 -lm 

globalvar.o: devpar.h ../com/globalvar.c
	cc ${CFLAGS} -I${DIR} -c ../com/globalvar.c


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
