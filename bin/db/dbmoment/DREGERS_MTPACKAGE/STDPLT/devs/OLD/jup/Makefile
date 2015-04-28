NAME= jup
DIR= ../jup
HEADERS= ../../h/igl.h devpar.h
SOURCE=	  open_close.c color.c vector.c  getspcpar.c initcolortable.c boxfill.c
LOC_OBJS= open_close.o color.o vector.o  getspcpar.o initcolortable.o \
	boxfill.o globalvar.o
COM_OBJS= ../com/main.o ../com/readcom.o ../com/line_point.o ../com/err.o \
	../com/symbol.o ../com/text.o ../com/getstdpar.o ../com/polygon.o \
	../com/label.o ../com/rastertext.o
DUM_OBJS= ../dum/raster.o ../dum/sendplot.o ../dum/pattern.o \
	  ../dum/newplot.o
SYM_OBJS=../../symboldata/getsymdata.o
CFLAGS=-g

${NAME}: ${LOC_OBJS} ${COM_OBJS} ${DUM_OBJS} ${SYM_OBJS}
	cc ${CFLAGS} -o ${NAME} ${LOC_OBJS} ${COM_OBJS} \
	${DUM_OBJS} ${SYM_OBJS}  /home/u1/dreger/Sources/libget.a -lm

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

cleanup:
	-rm -f $(LOC_OBJS) $(NAME)
