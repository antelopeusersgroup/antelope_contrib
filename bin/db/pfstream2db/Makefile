BIN=pfstream2db
LIB=
DLIB=$(LIB:.a=$(DSUFFIX))
DATADIR=
DATA=
INCLUDE=
PF=pfstream2db.pf

#MAN1=pfstream2db.1
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



OBJS=pfstream2db.o
$(BIN) : $(OBJS)
	$(RM) $@
	$(CC) $(CFLAGS) -o $@ $(OBJS) $(LDFLAGS) $(LDLIBS)
