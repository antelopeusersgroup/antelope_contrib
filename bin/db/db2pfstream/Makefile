BIN=db2pfstream
LIB=
DLIB=$(LIB:.a=$(DSUFFIX))
DATADIR=
DATA=
INCLUDE=
PF=db2pfstream.pf

MAN1=db2pfstream.1
MAN3=
MAN5=

CLEAN=

cflags=-g
fflags=
ldflags=
#ldlibs=-lglputil  -xlic_lib=sunperf,fui,fai,fsu $(DBLIBS)
#ldlibs=-lglputil  -xlic_lib=sunperf $(DBLIBS) -lpfstream -lbrttutil
ldlibs=-lglputil  -lperf $(DBLIBS) -lpfstream -lbrttutil

include $(ANTELOPEMAKE)

DIRS=



OBJS=db2pfstream.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
