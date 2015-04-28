# Makefile for symbol subroutines
# note this doesn't work
HEADERS = ../h/igl.h sym.h
SOURCE = getnames.c getsymdata.c
STD_OBJS = getnames.o
COM_OBJS = getsymdata.o
DATA = createfiles
#CFLAGS = -g -pg
#CFLAGS= -fsingle
CFLAGS= -arch x86_64 -Wno-implicit -Wno-implicit-int -Wno-return-type -Wno-format
CC=gcc

$(DATA):
	${CC} ${CFLAGS} -g -o createfiles createfiles.c ../../libget.a

getsymdata : $(STD_OBJS) $(DATA) $(COM_OBJS)
$(STD_OBJS) : $(HEADERS)
$(COM_OBJS) : $(HEADERS)

list:
	imprint makefile $(SOURCE) createfiles.c textfile sym.h

cleanup:
	-rm -f ${STD_OBJS} ${COM_OBJS} ${DATA}
