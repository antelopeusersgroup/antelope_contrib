INC_DIR = ${ANTELOPE}/include
LIB_DIR = /opt/SUNWspro/SC5.0/lib
LIB_DIR2 = ${ANTELOPE}/lib

DBLIBS = -lds -ldbl2 -ltr -lcoords -lposix4 -ltttaup -lresponse -lstock
GPLLIBS = -lgpl -lol -ltks -lgrx -lX11

####### EXECUTABLE FILE #######
EXEC =  mdcalc

####### SOURCE FILES #######
SRCS =  mdcalc.c

####### OBJECT FILES #######
OBJS =  mdcalc.o rms.o coda.o lstsqr.o

LIBS = ${DBLIBS} ${GPLLIBS} -lF77 -lM77 -lsunmath -lm

CC = cc
CFLAGS = -g
CFLAGS = $(cflags) -I$(INC_DIR)

####### COMPILE SOURCE FILES TO CREATE OBJECT FILES #######
.c :
	${CC} ${CFLAGS} -c $<

####### LINK OBJECT FILES TO CREATE EXECUTABLE #######
${EXEC} :       ${INCS} ${OBJS}
	${CC} -L ${LIB_DIR} -L ${LIB_DIR2} ${OBJS} ${LIBS} -o ${EXEC}
