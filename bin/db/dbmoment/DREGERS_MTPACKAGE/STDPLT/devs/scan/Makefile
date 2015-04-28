#
# Makefile for common routine for raster scan devices
#
HEADERS= ../../h/igl.h ../com/global.h
SOURCE= bits.c newplot.c vector.c linefill.c raster.c zap4.c boxfill.c
OBJS  = bits.o newplot.o vector.o linefill.o raster.o zap4.o boxfill.o
CFLAGS= -O -fsingle
PRINT=	lpr -p

scan:	$(OBJS)
	echo "changing scan"

$(OBJS): $(HEADERS)

list:
	${PRINT} makefile $(SOURCE)

cleanup:
	-rm -f $(OBJS)
