HEADERS= ../../h/igl.h devpar.h
SOURCE=	vector.c  getspcpar.c newplot.c color.c \
	poly.c raster.c rastext.c erase.c pattern.c getstdpar.c \
	sendplot.c opendev.c closedev.c initcolortable.c label.c
LOC_OBJS= vector.o globalvar.o open_close.o getspcpar.o newplot.o color.o \
	poly.o raster.o rastext.o erase.o pattern.o getstdpar.o \
	sendplot.o opendev.o closedev.o initcolortable.o label.o
COM_OBJS= ../com/main.o ../com/readcom.o ../com/line_point.o ../com/err.o \
	../com/text.o  ../com/polygon.o
#CFLAGS= -O -fsingle-precision-constant
CFLAGS= -O -Wno-return-type
PRINT=	lpr -p

dumsubs: ${LOC_OBJS}

$(LOC_OBJS): $(HEADERS)

list:
	${PRINT} makefile devpar.h $(SOURCE)

cleanup:
	-rm -f $(LOC_OBJS)
