#
# Makefile for common code
#
HEADERS= ../../h/igl.h global.h
SOURCE=	main.c readcom.c line_point.c err.c text.c getstdpar.c polygon.c \
	bits.c symbol.c label.c rastertext.c dot.c geth.c xpolygon.c
OBJS=	main.o readcom.o line_point.o err.o text.o getstdpar.o polygon.o \
	bits.o symbol.o label.o rastertext.o dot.o geth.o xpolygon.o
SYMDATA= symboldata.h
#CFLAGS= -O -fsingle-precision-constant
CFLAGS= -O -Wno-return-type
PRINT=	lpr -p

com:	$(OBJS) $(SYMDATA)
	echo "changing com"

$(OBJS): $(HEADERS)
$(SYMDATA):
	(cd ../../symboldata; make)

list:
	${PRINT} makefile $(HEADERS) globalvar.c $(SOURCE)

cleanup:
	-rm -f $(OBJS)
